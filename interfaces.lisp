#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

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

(defun find-implementation (interface &optional (system T))
  (unless (config-tree :interfaces)
    (load-config))
  (let* ((interface (interface interface))
         (configured-implementation (config-tree :interfaces (make-keyword (module-name interface)))))
    (unless configured-implementation
      (error 'interface-implementation-not-set :requested interface))
    (if system
        (asdf:find-system configured-implementation T)
        configured-implementation)))

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
    (handler-bind ((warning #'(lambda (warn) (muffle-warning warn))))
      (let* ((interface (interface interface)))
        #+:quicklisp
        (ql:quickload (find-implementation interface NIL))
        #-quicklisp
        (let ((implementation (find-implementation interface)))
          (unless (asdf:component-loaded-p implementation)
            (asdf:load-system implementation)))))))

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

(define-option-expander domain (package domain)
  `(setf (module-storage ,package :radiance-domain) ,(string-downcase domain)))

(defmethod domain ((module package))
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (or
     (module-storage module :radiance-domain)
     (string-downcase (module-name module)))))

(defmethod domain ((module symbol))
  (domain (module module)))

(define-option-expander permissions (package &rest perms)
  `(setf (module-storage ,package :radiance-permissions) ',perms))

(defun permissions (module)
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (module-storage module :radiance-permissions)))

(defun (setf permissions) (list module)
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (setf (module-storage module :radiance-permissions) list)))

(defun module-dependencies (module)
  (asdf:system-depends-on
   (if (typep module 'asdf:system)
       module
       (virtual-module (module-name module)))))

(defun module-required-interfaces (module)
  (loop for dep in (module-dependencies module)
        when (and (listp dep) (eql (first dep) :interface))
        collect (second dep)))

(defun module-required-systems (module)
  (loop for dep in (module-dependencies module)
        unless (and (listp dep) (eql (first dep) :interface))
        collect dep))

(defun describe-module (module)
  (let ((module (module module))
        (virtual (virtual-module (module-name module))))
    (format T "Module ~a

Domain: ~a
Implements: ~:[Nothing~;~:*~{~a~^, ~}~]
Configuration: ~:[None~;~:*~{~a~^, ~}~]
Permissions: ~:[None~;~:*~{~a~^, ~}~]~%"
            (module-name module)
            (domain module)
            (implements module)
            (let ((table (config-tree (module-name module))))
              (when table (loop for name being the hash-keys of table collect name)))
            (permissions module))    
    (if virtual
        (format T "~%System: ~a
Required interfaces: ~:[None~;~:*~{~a~^, ~}~]
Required systems: ~:[None~;~:*~{~a~^, ~}~]
Author: ~:[None~;~:*~a~]~@[
Description: ~a~]"
                (asdf:component-name virtual)
                (module-required-interfaces virtual)
                (module-required-systems virtual)
                (asdf:system-author virtual)
                (asdf:system-description virtual))
        (format T "No corresponding system found!"))))
