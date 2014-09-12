#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

;; Sets up a default trigger for pages
(define-page-option with-trigger (name uri body (value T))
  (assert (symbolp value))
  (if value
      (let ((name (if (eql value T) name value)))
        (values
         `((trigger ',name)
           ,@body)
         `(define-hook ,name ())))
      body))

(define-page-option uri-groups (name uri body uri-groups)
  (if uri-groups
      `((cl-ppcre:register-groups-bind ,uri-groups (,(path uri) (path *request*))
          ,@body))
      body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun transform-access-body (body branch)
    (if (not (eql branch T))
        `((unless (and (auth:current) (user:check (auth:current) ,branch))
            (error 'request-denied :message (format NIL "User ~a does not have the necessary rights ~s" (auth:current) ,branch)))
          ,@body)
        body)))

(define-page-option access (name uri body (branch T))
  (transform-access-body body branch))

(define-api-option access (name args body (branch T))
  (transform-access-body body branch))

(define-implement-hook (admin 'define-accessor-option)
  (admin:define-panel-option access (name category body (branch T))
    (declare (ignore name category))
    (transform-access-body body branch)))

(define-implement-hook (profile 'define-accessor-option)
  (profile:define-panel-option access (name category body (branch T))
    (declare (ignore name category))
    (transform-access-body body branch)))

;; Api catchall page
(define-api "" () (:documentation "API 404")
  (error 'api-call-not-found))

;; Api standard serialise
(defmethod api-serialize (object)
  (funcall *serialize-fallback* object))

(defmethod api-serialize ((list list))
  (mapcar #'api-serialize list))

(defmethod api-serialize ((err error))
  (list :object :error
        :type (type-of err)))

(defmethod api-serialize ((err radiance-error))
  (cons :message (cons (message err) (call-next-method))))

(defmethod api-serialize ((err request-error))
  (cons :request (cons (current-request err) (call-next-method))))

(defmethod api-serialize ((err api-argument-missing))
  (cons :argument (cons (argument err) (call-next-method))))

(defmethod api-serialize ((err api-argument-invalid))
  (cons :argument (cons (argument err) (call-next-method))))

(defmethod api-serialize ((err api-unknown-format))
  (cons :format (cons (requested-format err) (call-next-method))))

;; Api standard format
(define-api-format lisp (object)
  (setf (content-type *response*) "text/x-sexpr")
  (let ((*serialize-fallback* #'(lambda (o)
                                  (typecase o
                                    (hash-table
                                     (list :object :table
                                           :fields (loop for k being the hash-keys of o
                                                         for v being the hash-values of o
                                                         collect (cons k v))))
                                    (T o)))))
    (write-to-string
     (api-serialize object))))

;; Default urls
(define-page favicon (#@"/favicon.ico" 10) ()
  (serve-file (data-file "static/img/favicon.ico")))

(define-page robots (#@"/robots.txt" 10) ()
  (serve-file (data-file "static/txt/robots.txt")))

(define-page static (#@"/static/.*" 1000) ()
  (let ((slashpos (position #\/ (path *request*) :start (length "static/"))))
    (if slashpos
        (serve-file (static-file (subseq (path *request*) (1+ slashpos))
                                 (string-upcase (subseq (path *request*) (length "static/") slashpos))))
        (serve-file (merge-pathnames (subseq (path *request*) (length "static/")) (data-file "static/"))))))

(define-page welcome #@"/" ()
  (serve-file (data-file "static/html/hello.html")))
