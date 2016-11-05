#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defmethod documentation ((name symbol) (type (eql 'page)))
  (documentation name 'uri-dispatcher))

(defmethod (setf documentation) (string (name symbol) (type (eql 'page)))
  (setf (documentation name 'uri-dispatcher) string))

(defun remove-page (name)
  (remove-uri-dispatcher name))

(defmacro define-page (name uri options &body body)
  (destructuring-bind (uri &optional priority) (enlist uri)
    (multiple-value-bind (body forms) (expand-options 'page options name body uri)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@forms
         ,@(when (module)
             `((pushnew ',name (module-storage ,(module) 'radiance-pages))))
         (define-uri-dispatcher ,name (,uri ,priority)
           ,@body)))))

(define-delete-hook (module 'radiance-destroy-pages)
  (dolist (page (module-storage module 'radiance-pages))
    (remove-uri-dispatcher page)))
