#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(define-hook startup ())
(define-hook shutdown ())

(define-trigger startup ()
  (loop for module in (config-tree :startup)
        do (asdf:load-system module)))

(defun startup ()
  (load-config)
  (trigger 'startup))

(defun shutdown ()
  (trigger 'shutdown)
  (save-config))
