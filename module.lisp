#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module radiance-core
  (:use #:cl #:modularize #:modularize-interfaces #:modularize-hooks #:universal-config)
  (:nicknames #:radiance #:org.tymoonnext.radiance.lib.radiance.core)
  ;; re-export from modularize
  (:export
   #:define-module
   #:define-module-extension
   #:delete-module
   #:module
   #:module-p
   #:module-storage
   #:module-storage-remove
   #:module-identifier
   #:module-name
   #:current-module)
  ;; re-export from hooks
  (:export
   #:define-hook
   #:remove-hook
   #:define-trigger
   #:remove-trigger
   #:trigger)
  ;; re-export from interfaces
  (:export
   #:interface
   #:interface-p
   #:implementation
   #:reset-interface
   #:define-interface
   #:define-interface-extension
   #:defimpl
   #:i-defun
   #:i-defmacro
   #:i-defmethod)
  ;; toolkit.lisp
  (:export))
