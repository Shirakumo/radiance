#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(define-condition radiance-error (error)
  ((text :initarg :text :initform "Undefined Error")
   (code :initarg :code :initform -1))
  (:report (lambda (c s) (format s "~a: ~a (E~4d)" 
                                 (class-name (class-of c))
                                 (slot-value c 'text)
                                 (slot-value c 'code)))))

(define-condition module-already-initialized (radiance-error)
  ((module :initarg :module :reader module))
  (:report (lambda (c s) (format s "Module ~a already initialized!"
                                 (slot-value c 'module)))))

(define-condition interface-error (radiance-error)
  ((interface :initarg :interface :reader interface)))

(define-condition no-such-interface-error (interface-error) ()
  (:report (lambda (c s) (format s "Attempted to load interface ~a, which is not defined." (slot-value c 'interface)))))

(define-condition no-interface-implementation-error (interface-error) ()
  (:report (lambda (c s) (format s "Attempted to load interface ~a, but no implementation is configured for it." (slot-value c 'interface)))))

(define-condition interface-not-implemented-error (interface-error) ()
  (:report (lambda (c s) (format s "Attempted to access interface ~a, but no implementation is set!" (slot-value c 'interface)))))

(define-condition no-such-interface-function-error (interface-error)
  ((interface-function :initarg :interface-function :reader interface-function))
  (:report (lambda (c s) (format s "Attempted to define interface function ~a for ~a, which is not defined."
                                 (slot-value c 'interface-function) (slot-value c 'interface)))))

(define-condition error-page (radiance-error) ())
(define-condition auth-error (radiance-error) ())

(define-condition invalid (radiance-error) 
  ((var :initarg :var :initform "Unknown Variable")
   (validator :initarg :validator :initform "Unknown Validator"))
  (:report (lambda (c s) (format s "Variable ~a does not validate to ~a"
                                 (slot-value c 'var)
                                 (slot-value c 'validator)))))

(define-condition api-error (radiance-error)
  ((module :initarg :module :initform NIL)
   (apicall :initarg :apicall :initform NIL))
  (:report (lambda (c s) (format s "~a: ~a/~a: ~a" 
                                 (class-name (class-of c)) 
                                 (class-name (class-of (slot-value c 'module))) 
                                 (slot-value c 'apicall)
                                 (slot-value c 'text)))))

(define-condition api-args-error (api-error) ())
(define-condition api-auth-error (api-error) ())

(define-condition hook-error (radiance-error) ())

(define-condition namespace-conflict-error (hook-error)
  ((%namespace :initarg :namespace :initform (error "Conflicting namespace required.") :reader error-namespace))
  (:report (lambda (c s) (format s "Attempted to define namespace ~a, but it already exists." (slot-value c '%namespace)))))

(define-condition namespace-not-found-error (hook-error)
  ((%namespace :initarg :namespace :initform (error "Namespace required.") :reader error-namespace))
  (:report (lambda (c s) (format s "Attempted to operate on namespace ~a, but it does not exist." (slot-value c '%namespace)))))
