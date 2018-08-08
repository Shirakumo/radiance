#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-condition radiance-condition (condition)
  ((message :initarg :message :initform NIL :accessor message))
  (:report (lambda (c s) (format s "Condition of type ~s.~@[~%~a~]"
                                 (type-of c) (message c)))))

(define-condition radiance-error (error radiance-condition)
  ())

(define-condition radiance-warning (warning radiance-condition)
  ())

(define-condition definition-for-shared-package (style-warning radiance-warning)
  ((symbol :initarg :symbol :initform (error "SYMBOL required.")))
  (:report (lambda (c s) (format s "Definition on a symbol that is likely shared with other systems.~%~
                                    This definition may lead to unintended overrides.~@[~%~a~]"
                                 (message c)))))

(define-condition system-has-no-version (radiance-error)
  ((system :initarg :system :initform (error "SYSTEM required.")))
  (:report (lambda (c s) (format s "The system ~a has no version specified, so Radiance does not know how to migrate it to the latest point."
                                 (asdf:component-name (slot-value c 'system))))))

(define-condition backwards-migration-not-allowed (radiance-error)
  ((from :initarg :from :initform (error "FROM required."))
   (to :initarg :to :initform (error "TO required.")))
  (:report (lambda (c s) (format s "Cannot migrate a system backwards from ~a to ~a."
                                 (encode-version (slot-value c 'from)) (encode-version (slot-value c 'to))))))

(define-condition environment-not-set (radiance-error) ()
  (:report "The application environment was not yet set but is required.
This means you are either using Radiance for the first time or forgot to set it up properly.
In the first case, simply use the CONTINUE restart. In the second, make sure to adjust your
configuration and use the SETF ENVIRONMENT restart to set an explicit environment."))

(define-condition internal-error (radiance-error) ()
  (:report (lambda (c s) (format s "An internal error has ocurred.~@[ ~a~]" (message c)))))

(define-condition request-error (radiance-error)
  ((request :initarg :request :initform (when (boundp '*request*)
                                          (symbol-value '*request*))))
  (:report (lambda (c s) (format s "An error has ocurred while processing~@[ the request ~a~].~@[ ~a~]"
                                 (slot-value c 'request) (message c)))))

(define-condition request-empty (request-error) ()
  (:report (lambda (c s) (format s "The reply body was NIL on request ~a.~@[ ~a~]"
                                 (slot-value c 'request) (message c)))))

(define-condition request-not-found (request-error) ()
  (:report (lambda (c s) (format s "There was nothing that could handle the request ~a.~@[ ~a~]"
                                 (slot-value c 'request) (message c)))))

(define-condition request-denied (request-error) ()
  (:report (lambda (c s) (format s "Access denied.~@[ ~a~]"
                                 (message c)))))

(define-condition api-error (request-error) ()
  (:report (lambda (c s) (format s "The API call to ~a failed.~@[ ~a~]"
                                 (slot-value c 'request) (message c)))))

(define-condition api-auth-error (api-error) ()
  (:report (lambda (c s) (format s "The API call to ~a was denied.~@[ ~a~]"
                                 (slot-value c 'request) (message c)))))

(define-condition api-argument-missing (api-error)
  ((argument :initarg :argument :initform (error "ARGUMENT required.")))
  (:report (lambda (c s) (format s "The argument ~s is required, but was not passed.~@[ ~a~]"
                                 (slot-value c 'argument) (message c)))))

(define-condition api-argument-invalid (api-error)
  ((argument :initarg :argument :initform (error "ARGUMENT required.")))
  (:report (lambda (c s) (format s "The argument ~s is not valid.~@[ ~a~]"
                                 (slot-value c 'argument) (message c)))))

(define-condition api-call-not-found (api-error) ()
  (:report (lambda (c s) (format s "The requested api call address could not be found.~@[ ~a~]" (message c)))))

(define-condition api-unknown-format (api-error)
  ((format :initarg :format :initform (error "FORMAT required.")))
  (:report (lambda (c s) (format s "The requested format ~s is not known.~@[ ~a~]"
                                 (slot-value c 'format) (message c)))))

(define-condition api-unserializable-object (api-error)
  ((object :initarg :object :initform (error "OBJECT required.")))
  (:report (lambda (c s) (format s "The object ~s is not serializable to the API format."
                                 (slot-value c 'object)))))

(define-condition interface-condition (radiance-condition)
  ((interface :initarg :interface :initform (error "REQUESTED required."))))

(define-condition interface-implementation-not-set (interface-condition radiance-error)
  ()
  (:report (lambda (c s) (format s "Interface ~s requested but no implementation is configured."
                                 (slot-value c 'interface)))))

(define-condition interface-implementation-not-present (interface-condition radiance-error)
  ()
  (:report (lambda (c s) (format S "Interface ~s has no loaded implementation."
                                 (slot-value c 'interface)))))

(define-condition unparsable-uri-string (radiance-error)
  ((string :initarg :string :initform (error "STRING required.")))
  (:report (lambda (c s) (format s "The string ~s is not parseable as a URI."
                                 (slot-value c 'string)))))

(define-condition no-such-post-parameter (request-error)
  ((parameter :initarg :parameter :initform (error "PARAMETER required.")))
  (:report (lambda (c s) (format s "The post parameter ~s was not present."
                                 (slot-value c 'parameter)))))

(define-condition post-parameter-not-a-file (request-error)
  ((parameter :initarg :parameter :initform (error "PARAMETER required.")))
  (:report (lambda (c s) (format s "The post parameter ~s is not a posted file."
                                 (slot-value c 'parameter)))))

(define-condition file-to-serve-does-not-exist (request-error)
  ((file :initarg :file :initform (error "FILE required.")))
  (:report (lambda (c s) (format s "The file ~s was attempted to be served, but it does not exist."
                                 (slot-value c 'file)))))
