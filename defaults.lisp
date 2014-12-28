#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

;; Sets up a default trigger for pages
(define-page-option with-trigger (name uri body (value T))
  (assert (symbolp value))
  (if value
      (let ((name (if (eql value T) name value)))
        (values
         `((trigger '(,name ,*package*))
           ,@body)
         `(define-hook (,name ,*package*) ())))
      body))

(define-page-option uri-groups (name uri body uri-groups)
  (if uri-groups
      `((cl-ppcre:register-groups-bind ,uri-groups (,(path uri) (path (uri *request*)))
          ,@body))
      body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun transform-access-body (body branch)
    (if (not (eql branch T))
        `((unless (and (auth:current) (user:check (auth:current) ,branch))
            (error 'request-denied :message (format NIL "User ~a does not have the necessary rights ~s" (auth:current) ',branch)))
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
  (profile:define-panel-option access (name body (branch T))
    (declare (ignore name))
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
  (serve-file (data-file "static/favicon.ico")))

(define-page robots (#@"/robots.txt" 10) ()
  (serve-file (data-file "static/robots.txt")))

(define-page static (#@"/static/.*" 1000) ()
  (let* ((path (path (uri *request*)))
         (slashpos (position #\/ path :start (length "static/"))))
    (if slashpos
        (serve-file (static-file (subseq path (1+ slashpos))
                                 (string-upcase (subseq path (length "static/") slashpos))))
        (serve-file (merge-pathnames (subseq path (length "static/")) (data-file "static/"))))))

(define-page welcome #@"/^$" ()
  (serve-file (data-file "html/hello.html")))

;; Default routing to cut domains.
(defvar *domain-internalizers* ())

(defun compile-domain-internalizers (&optional (domains (config-tree :server :domains)))
  (loop for domain in domains
        collect (let ((parts (nreverse (cl-ppcre:split "\\." domain)))
                      (domain domain))
                  #'(lambda (uri)
                      (loop for subdomains = (domains uri) then (cdr subdomains)
                            for a = (car subdomains)
                            for b in parts
                            when (or (not subdomains) (not (string-equal a b)))
                            do (return NIL)
                            finally (return (values domain subdomains)))))))

(define-trigger (radiance:startup 'compile-domain-internalizers) ()
  (setf *domain-internalizers* (compile-domain-internalizers)))

(define-route internalizer (:mapping most-positive-fixnum) (uri)
  (loop for internalizer in *domain-internalizers*
        until (multiple-value-bind (domain subdomains) (funcall internalizer uri)
                (when domain
                  (when (boundp '*request*)
                    (setf (domain *request*) domain))
                  (setf (domains uri) subdomains)
                  T))))

(define-route externalizer (:reversal most-positive-fixnum) (uri)
  (cond
    ((boundp '*request*)
     (push (domain *request*) (domains uri))
     (unless (port uri)
       (setf (port uri) (port (uri *request*)))))
    ;; KLUDGE! Find a better way to do this...
    (T
     (push (first (uc:config-tree :server :domains)) (domains uri))
     (unless (port uri)
       (setf (port uri) (uc:config-tree :server :instances 0 :port))))))

(define-route virtual-module (:mapping 100000) (uri)
  (when (and (< 1 (length (path uri)))
             (string= "!/" (path uri) :end2 2))
    (let ((slashpos (position #\/ (path uri) :start 2)))
      (let ((module (subseq (path uri) 2 slashpos))
            (path (if slashpos
                      (subseq (path uri) (1+ slashpos))
                      "")))
        (when (boundp '*request*)
          (setf (field *request* 'virtual-module) module))
        (setf (path uri) path)
        (setf (domains uri) (append (domains uri) (list module)))))))

(define-route virtual-module (:reversal 100000) (uri)
  (when (and (boundp '*request*)
             (field *request* 'virtual-module))
    (setf (path uri) (concatenate 'string "!/" (field *request* 'virtual-module) "/" (path uri))
          (domains uri) (butlast (domains uri)))))
