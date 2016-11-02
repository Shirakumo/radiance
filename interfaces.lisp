#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

;; Subclass so we can have another AFTER method on ASDF:PERFORM
(defclass module (modularize:module) ())
;; Redefine function too since we shadowed that binding
(defun module (&optional module)
  (modularize:module module))

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

(defmacro future (package-designator symbol-name &rest args)
  (let ((symbol (gensym "SYMBOL")))
    `(let ((,symbol (find-symbol ,(string symbol-name) ,(string package-designator))))
       (when ,symbol
         (when (interface-p ,(string package-designator))
           (load-implementation ,(string package-designator)))
         (funcall ,symbol ,@args)))))

(defmethod asdf:perform :after ((op asdf:load-op) (module module))
  (loop for interface in (module-storage (module (virtual-module-name module)) :implements)
        do (trigger (find-symbol "IMPLEMENTED" (interface interface)))
           (future l debug :interfaces "~a now implemented by ~a" (module-name interface) (module-name (virtual-module-name module)))))

(defun find-implementation (interface &optional (system T))
  (check-environment)
  (let* ((interface (interface interface))
         (configured-implementation (config :interfaces (make-keyword (module-name interface)))))
    (unless configured-implementation
      (error 'interface-implementation-not-set :requested interface))
    ;; If quicklisp is available, the system might be loadable, but
    ;; may not have been installed yet. Check for this and install if
    ;; necessary.
    #+quicklisp
    (when (and system
               (not (asdf:find-system configured-implementation NIL)))
      (let ((ql-system (ql-dist:find-system configured-implementation)))
        (when (and ql-system (not (ql-dist:installedp ql-system)))
          (format T "~&Chaining Quicklisp to install interface ~a implementation ~a.~%"
                  (module-name interface) configured-implementation)
          (ql-dist:install ql-system))))
    (if system
        (asdf:find-system configured-implementation T)
        configured-implementation)))

(indent:define-indentation define-interface (4 &rest (&whole 2 0 4 &body)))

(defun load-implementation (interface)
  (let ((*load-verbose* nil)
        (*compile-verbose* nil)
        (*load-print* nil)
        (*compile-print* nil))
    (handler-bind ((warning #'(lambda (warn) (muffle-warning warn))))
      (let* ((interface (interface interface)))
        (let ((implementation (find-implementation interface NIL)))
          (unless (and (asdf:find-system implementation NIL)
                       (asdf:component-loaded-p
                        (asdf:find-system implementation)))
            #-quicklisp
            (asdf:load-system implementation)
            #+:quicklisp
            (ql:quickload implementation)))))))

;; FIXME: Currently we have no way of triggering the unimplemented hook...
(defmacro define-interface (name &body components)
  `(interfaces:define-interface ,name
     (define-hook-switch implemented unimplemented ())
     ,@components))

(defmacro define-implement-hook (interface &body body)
  (destructuring-bind (interface &optional (ident *package*)) (enlist interface)
    (let ((interface (interface interface))
          (hook (find-symbol "IMPLEMENTED" (interface interface))))
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         (define-trigger (,hook ,ident) ()
           (let ((*package* ,*package*)) ;; capture package env
             (funcall (compile NIL '(lambda () ,@body)))))))))
