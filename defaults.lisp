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

;; Standard serialisations
(defmethod api-serialize ((error radiance-error))
  (let ((table (make-hash-table)))
    (flet ((s (k v) (setf (gethash (string-downcase k) table) v)))
      (s :error-type (type-of error))
      (s :message (message error))
      (when (typep error 'request-error)
        (s :uri (uri-to-url (uri (current-request error)) :representation :external)))
      (when (typep error '(or api-argument-missing
                              api-argument-invalid))
        (s :argument (argument error)))
      (when (typep error 'api-unknown-format)
        (s :format (requested-format error)))
      (when (typep error 'user-error)
        (s :format (user error))))
    table))

;; Api catchall page
(define-api "" () (:documentation "API 404")
  (error 'api-call-not-found))

;; Api standard format
(define-api-format lisp (object)
  (when (boundp '*response*)
    (setf (content-type *response*) "text/x-sexpr"))
  (with-output-to-string (stream)
    (macrolet ((w (&rest args)
                 `(progn ,@(loop for arg in args
                                 collect (etypecase arg
                                           (string `(write-string ,arg stream))
                                           (character `(write-char ,arg stream))
                                           (list arg))))))
      (labels ((output (object)
                 (typecase object
                   ((or string symbol number)
                    (write object :stream stream))
                   (list
                    (w "("
                       (when object
                         (output (first object))
                         (loop for o in (rest object)
                               do (w " " (output o))))
                       ")"))
                   (vector
                    (w "#("
                       (when (< 0 (length object))
                         (output (aref object 0))
                         (loop for i from 1 below (length object)
                               do (w " " (output (aref object i)))))
                       ")"))
                   (hash-table
                    (w "#{"
                       (loop for i downfrom (hash-table-count object)
                             for k being the hash-keys of object
                             for v being the hash-values of object
                             do (w (output k) ": " (output v))
                                (unless (= i 1) (w ", ")))
                       "}"))
                   (T (output (api-serialize object))))))
        (output object)))))

(setf *default-api-format* "lisp")

;; Default urls
(define-page favicon (#@"/^favicon.ico$" 10) ()
  (serve-file (data-file "static/favicon.ico")))

(define-page robots (#@"/^robots.txt$" 10) ()
  (serve-file (data-file "static/robots.txt")))

(define-page static (#@"/^static/.*" 1000) ()
  (let* ((path (path (uri *request*)))
         (slashpos (position #\/ path :start (length "static/")))
         (file (if slashpos
                   (ignore-errors
                    (static-file (subseq path (1+ slashpos)) (string-upcase (subseq path (length "static/") slashpos))))
                   (merge-pathnames (subseq path (length "static/")) (data-file "static/")))))
    (if (and file (uiop:file-exists-p file))
        (serve-file file)
        (error 'request-not-found))))

(define-page welcome #@"/^$" ()
  (serve-file (data-file "html/hello.html")))

;; Default routing to cut domains.
(defvar *domain-internalizers* ())

(defun add-domain (domain)
  (pushnew domain (config :server :domains) :test #'string-equal)
  (compile-domain-internalizers)
  (config :server :domains))

(defun remove-domain (domain)
  (setf (config :server :domains) (remove domain (config :server :domains) :test #'string-equal))
  (compile-domain-internalizers)
  (config :server :domains))

(defun compile-domain-internalizers (&optional (domains (config :server :domains)))
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

(define-trigger (environment-change 'compile-domain-internalizers) ()
  (setf *domain-internalizers* (compile-domain-internalizers)))

(define-route internalizer (:mapping most-positive-fixnum) (uri)
  (loop for internalizer in *domain-internalizers*
        until (multiple-value-bind (domain subdomains) (funcall internalizer uri)
                (when domain
                  (when (boundp '*request*)
                    (setf (domain *request*) domain))
                  (setf (domains uri) subdomains)
                  T))))

(define-route externalizer (:reversal most-negative-fixnum) (uri)
  (cond
    ((boundp '*request*)
     (push (domain *request*) (domains uri))
     (unless (port uri)
       (setf (port uri) (port (uri *request*)))))
    ;; KLUDGE! Find a better way to do this...
    (T
     (push (first (config :server :domains)) (domains uri))
     (unless (port uri)
       (setf (port uri) (config :server :instances 0 :port))))))

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

(define-route virtual-module (:reversal -100000) (uri)
  (when (boundp '*request*)
    (let ((virtual (field *request* 'virtual-module))
          (last (car (last (domains uri)))))
      (when virtual
        ;; Check if we are operating on the requested virtual domain
        (cond ((string-equal virtual last)
               (setf (path uri) (concatenate 'string "!/" virtual "/" (path uri))
                     (domains uri) (butlast (domains uri))))
              (last
               ;; Otherwise we need to use the specified subdomain as virtual
               ;; since we're trying to cross-reference a resource.
               (setf (path uri) (concatenate 'string "!/" last "/" (path uri))
                     (domains uri) (butlast (domains uri))))
              (T
               ;; Static resource refer, no change.
               ))))))
