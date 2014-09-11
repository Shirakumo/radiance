#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(define-condition interface-implementation-not-set (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Interface ~s requested but no implementation is configured." (requested c)))))

(defclass interface-wrapper (asdf:system)
  ((interface :initarg :interface :initform (error "Interface required.") :accessor wrapped-interface)))

(defmethod asdf:perform ((op asdf::load-op) (wrapper interface-wrapper))
  (load-implementation (wrapped-interface wrapper)))

(defmethod asdf::resolve-dependency-combination ((module module) (combinator (eql :interface)) args)
  (make-instance
   'interface-wrapper
   :interface (first args)))

(defun find-implementation (interface)
  (unless (config-tree :interfaces)
    (load-config))
  (let* ((interface (interface interface))
         (configured-implementation (config-tree :interfaces (make-keyword (module-name interface)))))
    (unless configured-implementation
      (error 'interface-implementation-not-set :requested interface))
    (asdf:find-system configured-implementation T)))

(defmacro define-interface (name &body components)
  `(interfaces:define-interface ,name
     (defhook implemented ()
       "Called when the interface is implemented.")
     ,@components))

(indent:define-indentation define-interface (4 &rest (&whole 2 0 4 &body)))

(defun load-implementation (interface)
  (let ((*load-verbose* nil)
        (*compile-verbose* nil)
        (*load-print* nil)
        (*compile-print* nil))
    (let* ((interface (interface interface))
           (implementation (find-implementation interface)))
      (unless (asdf:component-loaded-p implementation)
        (asdf:load-system implementation)
        (trigger (find-symbol "IMPLEMENTED" interface))))))

(defmacro define-implement-hook (interface &body body)
  (destructuring-bind (interface &optional (ident *package*)) (if (listp interface) interface (list interface))
    `(define-trigger (,(find-symbol "IMPLEMENTED" (interface interface)) ,ident) ()
       (eval '(progn ,@body)))))
