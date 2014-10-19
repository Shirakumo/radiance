#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *page-options* (make-hash-table))

(defun page-option (name)
  (gethash name *page-options*))

(defun (setf page-option) (option name)
  (setf (gethash name *page-options*) option))

(defun remove-page-option (name)
  (remhash name *page-options*))

(define-options-definer define-page-option page-option (namevar urivar bodyvar valuevar))

(defmacro define-page (name uri options &body body)
  (destructuring-bind (uri &optional priority) (if (listp uri) uri (list uri))
    (multiple-value-bind (body forms) (expand-options *page-options* options body name uri)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@forms
         ,@(when (module)
             `((pushnew ',name (module-storage ,(module) 'radiance-pages))))
         (define-uri-dispatcher ,name (,uri ,(gensym "REQUEST") ,priority)
           (block ,name
             ,@body))))))

(define-delete-hook (module 'radiance-destroy-pages)
  (dolist (page (module-storage module 'radiance-pages))
    (remove-uri-dispatcher page)))
