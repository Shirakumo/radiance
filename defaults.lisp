#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

;; Sets up a default trigger for pages
(define-page-option with-trigger (name uri &optional (value T))
  (assert (symbolp value))
  (when value
    (let ((name (if (eql value T) name value)))
      (push `(trigger ',name) *page-body*)
      `(define-hook ,name ()))))

(define-page-option uri-groups (name uri &optional uri-groups)
  (when uri-groups
    (setf *page-body*
          `((cl-ppcre:register-groups-bind ,uri-groups (,(path uri) (path *request*))
              ,@*page-body*))))
  NIL)

(define-page-option access (name uri &optional branch)
  (when branch
    (setf *page-body*
          `((unless (and (auth:current) (user:check (auth:current) ,branch))
              (error 'request-denied))
            ,@*page-body*)))
  NIL)

(define-api-option access (name args &optional branch)
  (when branch
    (setf *page-body*
          `((unless (and (auth:current) (user:check (auth:current) ,branch))
              (error 'request-denied))
            ,@*page-body*)))
  NIL)

;; Api catchall page
(define-api "" () (:documentation "API 404")
  (error 'api-call-not-found))

;; Api standard serialise
(defmethod api-serialize (object)
  object)

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
  (write-to-string (api-serialize object)))

;; Default urls
(define-page favicon #@"/favicon.ico" ()
  (serve-file (data-file "static/img/favicon.ico")))

(define-page robots #@"/robots.txt" ()
  (serve-file (data-file "static/txt/robots.txt")))

(define-page static #@"/static/.*" ()
  (let ((slashpos (position #\/ (path *request*) :start (length "static/"))))
    (if slashpos
        (serve-file (static-file (subseq (path *request*) (1+ slashpos))
                                 (string-upcase (subseq (path *request*) (length "static/") slashpos))))
        (serve-file (merge-pathnames (subseq (path *request*) (length "static/")) (data-file "static/"))))))

(define-page welcome #@"/" ()
  (serve-file (data-file "static/html/hello.html")))
