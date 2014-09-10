#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(defvar *startup-time* NIL)

(define-hook startup ())
(define-hook shutdown ())

(defun startup ()
  (setf *startup-time* (get-universal-time))
  (load-config)
  (loop for module in (config-tree :startup)
        do (if (member :quicklisp *features*)
               (funcall (symbol-function (find-symbol "QUICKLOAD" :ql)) module)
               (asdf:load-system module)))
  (trigger 'startup))

(defun shutdown ()
  (trigger 'shutdown)
  (save-config)
  (setf *startup-time* NIL))

(defun uptime ()
  (- (get-universal-time) *startup-time*))
