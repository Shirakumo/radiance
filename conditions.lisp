#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defvar *debugger* NIL)

(define-condition radiance-error (error)
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


(defun handle-condition (condition)
  (if *debugger*
      (invoke-debugger condition)
      (invoke-restart 'set-data (format NIL "Error occurred: ~a" condition))))
