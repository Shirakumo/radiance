#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

;; Subclass so we can have another AFTER method on ASDF:OPERATE
(defclass module (modularize:module) ())
;; Redefine function too since we shadowed that binding
(defun module (&optional module)
  (modularize:module module))

(define-condition interface-implementation-not-set (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Interface ~s requested but no implementation is configured." (requested c)))))

;; Ho boy here we go!
;; Hack into parse-dependency-def and add the interface
;; since hooking into resolve-dependency-combination is not enough anymore.
;; sad faces all around.
(when (and (find-package :ASDF/PARSE-DEFSYSTEM) (find-symbol "PARSE-DEPENDENCY-DEF" :ASDF/PARSE-DEFSYSTEM))
  (eval
   `(progn
      (defvar *old-dependency-def-fun* (function ,(find-symbol "PARSE-DEPENDENCY-DEF" :ASDF/PARSE-DEFSYSTEM)))
      (defun ,(find-symbol "PARSE-DEPENDENCY-DEF" :ASDF/PARSE-DEFSYSTEM) (definition)
        (if (and (listp definition) (eql (car definition) :interface))
            definition
            (funcall *old-dependency-def-fun* definition))))))

(if (find-symbol "RESOLVE-DEPENDENCY-COMBINATION" :asdf)
    (eval
     `(defmethod ,(find-symbol "RESOLVE-DEPENDENCY-COMBINATION" :asdf) ((module module) (combinator (eql :interface)) args)
        (find-implementation (first args))))
    (error "Radiance cannot support this version of ASDF. Sorry!"))

(defmethod asdf:operate :after ((op asdf:load-op) (module module) &key)
  (loop for interface in (module-storage (module (virtual-module-name module)) :implements)
        do (trigger (find-symbol "IMPLEMENTED" (interface interface)))))

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
        (asdf:load-system implementation)))))

(defmacro define-implement-hook (interface &body body)
  (destructuring-bind (interface &optional (ident *package*)) (if (listp interface) interface (list interface))
    (let ((interface (interface interface))
          (hook (find-symbol "IMPLEMENTED" (interface interface))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (define-trigger (,hook ,ident) ()
           (let ((*package* ,*package*)) ;; capture package env
             (funcall (compile NIL '(lambda () ,@body)))))
         ,@(when (implementation interface)
             `((trigger ',hook)))))))
