#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *startup-time* NIL)
(defvar *running* NIL)

(define-hook server-start ())
(define-hook server-ready ())
(define-hook server-stop ())
(define-hook server-shutdown ())
(define-hook startup ())
(define-hook startup-done ())
(define-hook shutdown ())
(define-hook shutdown-done ())

(defun startup (&optional application)
  (when *running*
    (error "Radiance is already running!"))
  
  (setf *startup-time* (get-universal-time))
  (load-config (if application
                   (asdf:system-relative-pathname application "radiance.uc" :type "lisp")
                   *config-path*))
  (loop for module in (config-tree :startup)
        do (if (member :quicklisp *features*)
               (funcall (symbol-function (find-symbol "QUICKLOAD" :ql)) module)
               (asdf:load-system module)))

  (trigger 'startup)
  
  (unless (implementation 'logger)
    (load-implementation 'logger))
  (l:info :radiance "Starting up.")
  (l:info :radiance "Ensuring prerequisites.")
  (unless (implementation 'server)
    (load-implementation 'server))

  (l:info :radiance "Starting server.")
  (trigger 'server-start)
  (setf *running* T)
  (trigger 'server-ready)

  (trigger 'startup-done)
  (l:info :radiance "Startup done.")
  T)

(defun shutdown ()  
  (unless *running*
    (error "Radiance is not running!"))
  
  (trigger 'shutdown)
  
  (l:info :radiance "Stopping server.")
  (trigger 'server-stop)
  (setf *running* NIL)
  (trigger 'server-shutdown)
  
  (save-config)
  (setf *startup-time* NIL)

  (trigger 'shutdown-done)
  (l:info :radiance "Shutdown done.")
  T)

(defun uptime ()
  (- (get-universal-time) *startup-time*))

(defun started-p ()
  *running*)
