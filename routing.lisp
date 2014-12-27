#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *routes* ())

(defun ensure-path (var)
  (if (listp var) (format NIL "~{~a~^/~}" var) var))

(defun ensure-list (var)
  (if (listp var) var (list var)))

(defclass route ()
  ((name :initarg :name :initform (error "NAME required.") :accessor name)
   (type :initarg :type :initform (error "TYPE required.") :accessor route-type)
   (priority :initarg :priority :initform 0 :accessor priority)
   (translator :initarg :translator :initform (error "TRANSLATOR required.") :accessor translator)))

(defmethod print-object ((route route) stream)
  (print-unreadable-object (route stream :type T)
    (format stream "~s ~a" (route-type route) (name route)))
  route)

(defun route (name type)
  (find-if #'(lambda (route)
               (and (eql type (route-type route))
                    (eql name (name route))))
           *routes*))

(defun (setf route) (translator name type &optional priority)
  (let ((route (route name type)))
    (if route
        (setf (priority route) (or priority (priority route))
              (translator route) translator)
        (push (make-instance 'route :name name :type type :translator translator :priority (or priority 0)) *routes*)))
  (setf *routes* (sort *routes* #'> :key #'priority))
  (route name type))

(defun remove-route (name type)
  (let ((found NIL))
    (setf *routes*
          (delete-if #'(lambda (route)
                         (when (and (eql type (route-type route))
                                    (eql name (name route)))
                           (setf found route)))
                     *routes*))
    found))

(defmacro define-route (name type (urivar) &body body)
  (destructuring-bind (name &optional priority) (ensure-list name)
    `(setf (route ',name ,type ,priority)
           #'(lambda (,urivar) ,@body))))

(defun extract-vars-and-tests (test-forms)
  (loop with basic-tests = ()
        with regex-tests = ()
        for test in test-forms
        collect (etypecase test
                  (symbol test)
                  (fixnum
                   (let ((gens (gensym (format NIL "FIXNUM=~a" test))))
                     (push (list '=  gens test) basic-tests)
                     gens))
                  (string
                   (let ((gens (gensym (format NIL "STRING=~a" test))))
                     (push (list 'string= gens test) basic-tests)
                     gens))
                  (list
                   (let ((gens (gensym (format NIL "REGEX=~a" (car test)))))
                     (push (list gens (car test) (cdr test)) regex-tests)
                     gens))) into vars
        finally (return (values vars basic-tests regex-tests))))

(defmacro with-destructuring-route-bind (test-form value-form &body body)
  (multiple-value-bind (vars basic-tests regex-tests) (extract-vars-and-tests test-form)
    `(ignore-errors ; To ensure that an unmatching form just skips silently.
      ;; I'd love a better mechanism than ignore-errors, since it will also
      ;; catch other errors, but the CLHS does not ensure any form of error
      ;; that we could specifically catch on a destructuring-bind fail.
      ;;
      ;; This problem has hit me many times before and it always makes me sad.
      ;; Imagine crying kittens. 
      (destructuring-bind ,vars ,value-form
        (when (and ,@(loop for (func var comparison) in basic-tests
                           collect `(,func ,comparison ,var)))
          ,@(loop with body = body
                  for (var regex bindings) in regex-tests
                  do (setf body `((cl-ppcre:register-groups-bind ,bindings (,regex ,var)
                                    ,@body)))
                  finally (return body)))))))

(defmacro with-route-part-bindings ((value-form test-form) &body body)
  (cond ((eq test-form '*)
         `(progn ,@body))
        ((integerp test-form)
         `(when (= ,test-form ,value-form)
            ,@body))
        ((stringp test-form)
         `(when (string= ,test-form ,value-form)
            ,@body))
        ((listp test-form)
         `(with-destructuring-route-bind ,test-form ,value-form
            ,@body))
        ((symbolp test-form)
         `(let ((,test-form ,value-form))
            ,@body))
        (T (error "I don't know what to do with the test-form ~s." test-form))))

(defmacro with-route-test-bindings ((uri domains port path) &body body)
  `(with-route-part-bindings ((or (port ,uri) -1) ,port)
     (with-route-part-bindings ((domains ,uri) ,domains)
       (with-route-part-bindings ((cl-ppcre:split "/" (path ,uri)) ,path)
         ,@body))))

(defmacro define-matching-route (name type (urivar domains port path) &body body)
  `(define-route ,name ,type (,urivar)
     (with-route-test-bindings (,urivar ,domains ,port ,path)
       ,@body)))

(defmacro define-target-route (name type (domains port path) (target-domains target-port target-path))
  `(define-matching-route ,name ,type (uri ,domains ,port ,path)
     ,@(unless (eql target-domains '*)
         `((setf (domains uri)
                 ,(if (listp target-domains)
                      `(apply #'append (mapcar #'ensure-list (list ,@target-domains)))
                      `(ensure-list ,target-domains)))))
     ,@(unless (eql target-port '*)
         `((setf (port uri) ,target-port)))
     ,@(unless (eql target-path '*)
         `((setf (path uri)
                 ,(if (listp target-path)
                      `(format NIL "~{~a~^/~}" (mapcar #'(lambda (a) (ensure-path a)) (list ,@target-path)))
                      `(ensure-path ,target-path)))))))

(defun escape-regex-dots-not-in-group (regex)
  (with-output-to-string (s)
    (loop with parencount = 0
          for prev = #\Space then char
          for char across regex
          do (cond ((and (char= char #\() (char/= prev #\\))
                    (incf parencount))
                   ((and (char= char #\)) (char/= prev #\\))
                    (decf parencount))
                   ((and (char= char #\.) (= 0 parencount))
                    (write-char #\\ s)))
             (write-char char s))))

(defmacro define-string-route (name type source target)
  (let ((source (escape-regex-dots-not-in-group source)))
    `(define-route ,name ,type (uri)
       (let ((uristring (uri-to-string uri :print-port T)))
         (when (cl-ppcre:scan ,source uristring)
           (let ((newuri (parse-uri (cl-ppcre:regex-replace ,source uri ,target))))
             (setf (domains uri) (domains newuri)
                   (port uri) (port newuri)
                   (path uri) (path newuri))))))))

(defun internal-uri (uri)
  "Returns an internal equivalent of URI, suitable for usage inside the Radiance framework environment.

See RADIANCE>ROUTING>MAPPING"
  (loop with internal = (copy-uri uri)
        for route in *routes*
        when (eql (route-type route) :mapping)
        do (funcall (translator route) internal)
        finally (return internal)))

(defun external-uri (uri)
  "Returns an external equivalent of URI, suitable for usage in templates and user-facing data.

See RADIANCE>ROUTING>REVERSAL"
  (loop with external = (copy-uri uri)
        for route in *routes*
        when (eql (route-type route) :reversal)
        do (funcall (translator route) external)
        finally (return external)))
