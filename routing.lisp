#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

;;; Route spec
;; (DOMAINS PORT PATHS)
;; DOMAINS   ::= LIST-SPEC | *
;; PORT      ::= fixnum | symbol | *
;; PATHS     ::= LIST-SPEC | *
;; LIST-SPEC ::= VAR* [&optional OPT*] [&rest symbol]
;; VAR       ::= symbol | string
;; OPT       ::= symbol | string | (symbol value)
;;
;; The * symbol at any part means that no test is
;; made.
;;
;; Ports are either skipped, bound to the symbol
;; or tested for = equality with the given fixnum.
;;
;; Domains are matched via a pseudo lambda-list
;; destructuring that supports &OPTIONAL, &REST and
;; string restraints.
;;
;; Paths are split by / and then matched the same way
;; as domains.
;;
;; A symbol in this context means that the corresponding
;; part will be bound to that symbol and passed to the
;; transformer. A string means that the URI only matches
;; the route if the corresponding part is string= to the
;; string.

(defvar *routes* (make-hash-table :test 'eql))

(defclass route ()
  ((domains :initarg :domains :initform '* :accessor domains)
   (port :initarg :port :initform '* :accessor port)
   (path :initarg :path :initform '* :accessor path)
   (matcher :initarg :matcher :accessor matcher)
   (transformer :initarg :transformer :initform (error "TRANSFORMER required.") :accessor transformer)))

(defmethod initialize-instance :after ((route route) &key)
  (setf (matcher route) (compile-route-matcher route)))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type T)
    (format stream "(~a ~a ~a)" (domains route) (port route) (path route)))
  route)

(defmacro %setf-wrapper (slot)
  `(defmethod (setf ,slot) (val (route route))
     (setf (slot-value route ',slot) val
           (matcher route) (compile-route-matcher route))))
(%setf-wrapper domains)
(%setf-wrapper port)
(%setf-wrapper path)

(defun route (name)
  (assert (symbolp name))
  (gethash name *routes*))

(defun (setf route) (route name)
  (assert (typep route 'route))
  (assert (symbolp name))
  (setf (gethash name *routes*) route))

(defun route-uri (route uri)
  (funcall (matcher route) uri))

(defun extract-symbols (list)
  (when (listp list)
    (loop for item in list
          when (or (symbolp item) (listp item))
            collect (if (listp item) (car item) item))))

(defvar *matched-vars*)
(defun put-key (key val)
  (push val *matched-vars*)
  (push (make-keyword key) *matched-vars*))

(defun compile-port-test (port-matcher)
  (cond
    ((eql port-matcher '*)
     (constantly T))
    ((typep port-matcher 'fixnum)
     #'(lambda (uri)
         (and (port uri) (= (port uri) port-matcher))))
    ((symbolp port-matcher)
     #'(lambda (uri)
         (put-key port-matcher (port uri))
         T))
    (T (error "Invalid port matcher ~s, should be a symbol, '* or fixnum." port-matcher))))

(defun compile-list-test (list)
  ;; check for conformity and compile tests
  (let ((tests ())
        (required-count (loop for i in list until (member i '(&optional &rest)) count i))
        (optional-count (loop for i in list until (member i '(&rest)) unless (eql i '&optional) count i))
        (in-rest)
        (in-optional))
    (dolist (item list)
      (when (member item '(&allow-other-keys &aux &body &environment &key &whole))
        (error "Only the &REST and &OPTIONAL lambda-keywords are allowed."))
      (when in-rest
        (unless (symbolp item)
          (error "The &REST argument must be a symbol."))
        (if (eql in-rest T)
            (setf in-rest item
                  tests (cons #'(lambda (a) (put-key item a)) tests))
            (error "Only one symbol can follow after &REST.")))
      (when (and (not in-optional) (not (stringp item)) (not (symbolp item)))
        (error "Required arguments must be either a STRING or a SYMBOL."))
      (when in-optional
        (unless (or (stringp item) (symbolp item) (listp item))
          (error "Optional arguments must be either a STRING, SYMBOL or a LIST."))
        (when (and (listp item) (stringp (first item)))
          (error "Default values make no sense for STRING matches."))
        (if (stringp item)
            (push #'(lambda (a)
                      (let ((a (car a)))
                        (or (not a) (string= a item)))) tests)
            (destructuring-bind (item &optional default) (if (listp item) item (list item))
              (push #'(lambda (a)
                        (let ((a (car a)))
                          (put-key item (or a default)))) tests))))
      (when (eql item '&rest) (setf in-rest T))
      (when (eql item '&optional) (setf in-optional T))
      (unless (or in-optional in-rest)
        (if (stringp item)
            (push #'(lambda (a) (string= (car a) item)) tests)
            (push #'(lambda (a) (let ((a (car a))) (put-key item a))) tests))))
    (setf tests (nreverse tests))
    #'(lambda (paths)
        (when (or (and in-optional
                       (<= (length paths) optional-count))
                  (and in-rest
                       (<= required-count (length paths)))
                  (= required-count (length paths)))
          (loop for test in tests
                for path = paths
                  then (cdr path)
                always (funcall test path))))))

(defun compile-domain-test (domains)
  (cond
    ((eql domains '*)
     (constantly T))
    ((listp domains)
     (let ((test (compile-list-test domains)))
       #'(lambda (uri)
           (funcall test (domains uri)))))
    (T (error "Invalid domain matcher ~s, should be either '* or a matcher-lambda-list." domains))))

(defun compile-path-test (paths)
  (cond
    ((eql paths '*)
     (constantly T))
    ((listp paths)
     (let ((test (compile-list-test paths)))
       #'(lambda (uri)
           (funcall test (cl-ppcre:split "/" (path uri))))))
    (T (error "Invalid path matcher ~s, should be either '* or a matcher-lambda-list." paths))))

(defun compile-route-matcher (route)
  (let ((port-test (compile-port-test (port route)))
        (domain-test (compile-domain-test (domains route)))
        (path-test (compile-path-test (path route))))
    #'(lambda (uri)
        (let ((*matched-vars*))
          (when (and (funcall port-test uri)
                     (funcall domain-test uri)
                     (funcall path-test uri))
            (apply (transformer route) uri *matched-vars*))))))

(defmacro define-route (name pattern &body transformations)
  (destructuring-bind (name &optional (uri 'uri)) (if (listp name) name (list name))
    (destructuring-bind (domains port path) pattern
      (let ((symbols (append (extract-symbols domains)
                             (when (and (symbolp port) (not (eql port '*)))
                               (list port))
                             (extract-symbols path))))
        `(setf (route ',name)
               (make-instance
                'route
                :path ',path
                :domains ',domains
                :port ,(if (symbolp port) `',port port)
                :transformer #'(lambda (,uri &key ,@symbols)
                                 ,@transformations)))))))

(defun resolve-route (uri)
  (loop for route being the hash-values of *routes*
        for match = (route-uri route uri)
        when match
          do (return (resolve-route match))
        finally (return uri)))
