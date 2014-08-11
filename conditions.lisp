#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defvar *debugger* NIL)

(define-condition radiance-error (error)
  ((message :initarg :message :initform NIL :accessor message)))

(define-condition radiance-warning (warning)
  ((message :initarg :message :initform NIL :accessor message)))

(define-condition internal-error (radiance-error) ()
  (:report (lambda (c s) (format s "An internal error has ocurred.~@[ ~a~]" (message c)))))

(define-condition request-error (radiance-error)
  ((current-request :initarg :request :initform *request* :accessor current-request))
  (:report (lambda (c s) (format s "An error has ocurred while processing the request ~a.~@[ ~a~]"
                                 (current-request c) (message c)))))

(define-condition request-empty (request-error) ()
  (:report (lambda (c s) (format s "The reply body was NIL on request ~a.~@[ ~a~]"
                                 (current-request c) (message c)))))

(define-condition request-not-found (request-error) ()
  (:report (lambda (c s) (format s "There was nothing that could handle the request ~a.~@[ ~a~]"
                                 (current-request c) (message c)))))

(define-condition api-error (request-error) ()
  (:report (lambda (c s) (format s "The API call to ~a failed.~@[ ~a~]"
                                 (current-request c) (message c)))))

(define-condition api-argument-missing (api-error)
  ((argument :initarg :argument :initform (error "ARGUMENT required.") :accessor argument))
  (:report (lambda (c s) (format s "The argument ~s is required, but was not passed.~@[ ~a~]"
                                 (argument c) (message c)))))

(define-condition api-argument-invalid (api-error)
  ((argument :initarg :argument :initform (error "ARGUMENT required.") :accessor argument))
  (:report (lambda (c s) (format s "The argument ~s is not valid.~@[ ~a~]"
                                 (argument c) (message c)))))

(define-condition api-call-not-found (api-error) ()
  (:report (lambda (c s) (format s "The requested api call address could not be found.~@[ ~a~]" (message c)))))

(define-condition api-response-empty (api-error) ()
  (:report (lambda (c s) (format s "The API response was empty.~@[ ~a~]" (message c)))))

(define-condition api-unknown-format (api-error)
  ((requested-format :initarg :format :initform (error "FORMAT required.") :accessor requested-format))
  (:report (lambda (c s) (format s "The requested format ~s is not known.~@[ ~a~]"
                                 (requested-format c) (message c)))))

(define-condition database-error (radiance-error) ())

(define-condition database-warning (radiance-warning) ())

(define-condition database-connection-failed (database-error)
  ((database :initarg :database :initform (error "DATABASE required.") :accessor database))
  (:report (lambda (c s) (format s "Failed to connect to database ~a.~@[ ~a~]"
                                 (database c) (message c)))))

(define-condition database-connection-already-open (database-warning)
  ((database :initarg :database :initform (error "DATABASE required.") :accessor database))
  (:report (lambda (c s) (format s "Connection to database ~a already open.~@[ ~a~]"
                                 (database c) (message c)))))

(define-condition database-invalid-collection (database-error)
  ((collection :initarg :collection :initform (error "COLLECTION required.") :accessor collection))
  (:report (lambda (c s) (format s "No such collection ~s.~@[ ~a~]"
                                 (collection c) (message c)))))

(define-condition database-collection-already-exists (database-error)
  ((collection :initarg :collection :initform (error "COLLECTION required.") :accessor collection))
  (:report (lambda (c s) (format s "The collection ~s already exists.~@[ ~a~]"
                                 (collection c) (message c)))))

(define-condition database-invalid-field (database-error)
  ((fielddef :initarg :fielddef :initform (error "FIELD required.") :accessor fielddef))
  (:report (lambda (c s) (format s "The field declaration ~s is invalid.~@[ ~a~]"
                                 (fielddef c) (message c)))))

(define-condition data-model-not-inserted-yet (database-error)
  ((model :initarg :model :initform (error "MODEL required.") :accessor model))
  (:report (lambda (c s) (format s "The model ~s has not been inserted yet.~@[ ~a~]"
                                 (model c) (message c)))))

(defun handle-condition (condition)
  (if *debugger*
      (invoke-debugger condition)
      (invoke-restart 'set-data (format NIL "Error occurred: ~a" condition))))
