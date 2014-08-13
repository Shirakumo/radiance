#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

;; Sets up a default trigger for pages
(define-page-option with-trigger (name uri &optional (value T))
  (declare (ignore uri))
  (assert (symbolp value))
  (when value
    (let ((name (if (eql value T) name value)))
      (push `(trigger ',name) *page-body*)
      `(define-hook ,name ()))))

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
  (write-to-string (api-serialize object) :readably T))

;; Default urls
(define-page favicon ^u"/favicon.ico" ()
  (serve-file (static-file "img/favicon.ico")))

(define-page robots ^u"/robots.txt" ()
  (serve-file (static-file "txt/robots.txt")))

(define-page static ^u"/static/.*" ()
  (serve-file (static-file (subseq (path *request*)
                                   (1+ (position #\/ (path *request*)))))))

(define-page welcome ^u"/" ()
  (serve-file (static-file "html/hello.html")))
