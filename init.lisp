#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(define-hook startup ())
(define-hook shutdown ())

(defun startup ()
  (load-config)
  (loop for module in (config-tree :startup)
        do (if (member :quicklisp *features*)
               (funcall (symbol-function (find-symbol "QUICKLOAD" :ql)) module)
               (asdf:load-system module)))
  (trigger 'startup))

(defun shutdown ()
  (trigger 'shutdown)
  (save-config))
