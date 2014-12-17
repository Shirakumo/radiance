#|
This file is a part of Radiance
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

;;;;;;
;; PATTERN syntax
;;
;; uri       ::= [domain] [port] "/" [path]
;; domain    ::= (subdomain ".")* subdomain
;; subdomain ::= alphas | subst | resource
;; port      ::= ":" (number | subst | resource)
;; path      ::= mixed | resource
;; mixed     ::= (string | subst)*
;; subst     ::= "{" (!"}")* "}"
;; resource  ::= "<" target [":" name [";" args]] ">"
;; target    ::= alphas
;; name      ::= alphas
;; args      ::= ((!","|">")* ",")* (!","|">")
;;

(defvar *args* ())
(define-matcher letters (or (in #\a #\z) (in #\A #\Z)))
(define-matcher number (in #\0 #\9))
(define-matcher alpha (or :letters :number (is #\-)))

(defclass pattern (uri)
  ())

(defgeneric resolve (pattern &rest args)
  (:method (thing &rest args)
    (declare (ignore args))
    thing)
  (:method ((pattern pattern) &rest args)
    (let ((*args* (or args *args*)))
      (make-uri :domains (mapcar #'resolve (domains pattern))
                :port (resolve (port pattern))
                :path (if (listp (path pattern))
                          (format NIL "~{~a~}" (mapcar #'resolve (path pattern)))
                          (resolve (path pattern)))))))

(defmethod make-load-form ((pattern pattern) &optional env)
  (declare (ignore env))
  (flet ((transform (a)
           (etypecase a
             ((or string fixnum null) a)
             ((or placeholder resource) (make-load-form a)))))
    `(make-instance 'pattern :domains (list ,@(mapcar #'transform (domains pattern)))
                             :port ',(transform (port pattern))
                             :path ,(if (listp (path pattern))
                                        `(list ,@(mapcar #'transform (path pattern)))
                                        (transform (path pattern)))
                             :matcher NIL)))

(defmethod print-object ((pattern pattern) stream)
  (if *print-readably*
      (let ((*print-readably* NIL))
        (format stream "#?\"~{~a~^.~}~@[:~a~]/~@[~a~]\""
                (reverse (domains pattern)) (port pattern) (path pattern)))
      (print-object (resolve pattern) stream))
  pattern)

(defclass resource ()
  ((target :initarg :target :initform (error "TARGET required.") :accessor target)
   (name :initarg :name :initform :domain :accessor name)
   (args :initarg :args :initform () :accessor args)))

(defmethod resolve ((resource resource) &rest args)
  (declare (ignore args))
  (apply #'resource (name resource) (target resource) (args resource)))

(defmethod make-load-form ((resource resource) &optional env)
  (declare (ignore env))
  `(make-instance 'resource :target ,(target resource)
                            :name ,(name resource)
                            :args ',(args resource)))

(defmethod print-object ((resource resource) stream)
  (if *print-readably*
      (print (make-load-form resource) stream)
      (format stream "<~a:~a~@[;~{~a~^,~}~]>"
              (target resource) (name resource) (args resource))))

(defun read-resource-target ()
  (string-upcase (consume-until (make-matcher (any #\> #\:)))))

(defun read-resource-name ()
  (when (char= (peek) #\:)
    (advance)
    (consume-until (make-matcher (any #\> #\;)))))

(defun read-resource-args ()
  (when (char= (peek) #\;)
    (advance)
    (loop for peek = (peek)
          until (or (not peek) (char= peek #\>))
          do (when (char= peek #\,) (advance))
          collect (consume-until (make-matcher (any #\> #\,))))))

(defun read-resource ()
  (when (char= (or (peek) #\ ) #\<)
    (advance) ;; skip opening <
    (let ((module (read-resource-target))
          (name (read-resource-name))
          (args (read-resource-args)))
      (advance) ;; skip closing >
      (unless (module-p module)
        (warn "No module or interface ~a known, but used as resource identifier in URI." module))
      (make-instance 'resource :target module :name (or* name :domain) :args args))))

(defclass placeholder ()
  ((var :initarg :var :initform (error "VAR required.") :accessor var)))

(defmethod resolve ((placeholder placeholder) &rest args)
  (declare (ignore args))
  (let ((var (var placeholder)))
    (etypecase var
      (fixnum (nth var *args*))
      (keyword (getf *args* var)))))

(defmethod make-load-form ((placeholder placeholder) &optional env)
  (declare (ignore env))
  `(make-instance 'placeholder :var ,(var placeholder)))

(defmethod print-object ((placeholder placeholder) stream)
  (if *print-readably*
      (print (make-load-form placeholder) stream)
      (format stream "{~a}" (var placeholder))))

(defun read-substitute ()
  (when (char= (or (peek) #\ ) #\{)
    (advance) ;; skip opening {
    (let* ((contents (consume-until (make-matcher (is #\}))))
           (keyword (or (ignore-errors (parse-integer contents))
                        (intern (string-upcase contents) "KEYWORD"))))
      (advance) ;; skip closing }
      (make-instance 'placeholder :var keyword))))

(defun read-mixed ()
  (loop for peek = (peek)
        while peek
        collect (case peek
                  (#\{ (read-substitute))
                  (T (consume-until (make-matcher (is #\{)))))))

(defun read-path ()
  (when (or (char= (or (peek) #\ ) #\/)
            (error "Path / expected."))
    (advance) ;; skip beginning /
    (or (read-resource)
        (read-mixed))))

(defun read-port ()
  (when (char= (or (peek) #\ ) #\:)
    (advance) ;; skip beginning :
    (or (read-substitute)
        (read-resource)
        (consume-until (make-matcher (not :number))))))

(defun read-subdomain ()
  (or (read-substitute)
      (read-resource)
      (consume-until (make-matcher (not :alpha)))))

(defun read-domains ()
  (loop with domains = ()
        while (funcall (make-matcher (or (any #\. #\< #\{) :alpha)))
        do (when (char= (peek) #\.) (advance))
           (push (read-subdomain) domains)
        finally (return domains)))

(defun read-pattern ()
  (let ((domains (read-domains))
        (port (read-port))
        (path (read-path)))
    (make-instance
     'pattern :domains domains :port port :path path :matcher NIL)))

(defun parse-pattern (string)
  (with-lexer-environment (string)
    (read-pattern)))

(set-dispatch-macro-character #\# #\? #'(lambda (stream char arg)
                                          (declare (ignore char arg))
                                          (parse-pattern (read stream))))
