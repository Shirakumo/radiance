#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *startup-time* NIL)
(defvar *running* NIL)

(define-hook-switch server-start server-shutdown ())
(define-hook-switch server-ready server-stop ())
(define-hook-switch startup shutdown ())
(define-hook startup-done ())
(define-hook shutdown-done ())

(defun startup (&optional (environment (or *environment* "default")))
  (check-type environment string)
  
  (when *running*
    (error "Radiance is already running!"))

  (setf *startup-time* (get-universal-time))
  (setf (environment) environment)

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

  (l:info :radiance "Loading startup modules.")
  (dolist (module (config :startup))
    #+quicklisp (ql:quickload module)
    #-quicklisp (asdf:load-system module))

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
  
  (setf *startup-time* NIL)

  (trigger 'shutdown-done)
  (l:info :radiance "Shutdown done.")
  T)

(defun uptime ()
  (when *startup-time*
    (- (get-universal-time) *startup-time*)))

(defun started-p ()
  *running*)
