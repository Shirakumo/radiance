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
  
  ;; Migrate Radiance on its own if necessary.
  (migrate 'radiance-core T T)

  (setf *startup-time* (get-universal-time))
  (setf (environment) environment)

  (unless (boundp '*debugger*)
    (setf *debugger* (config :debugger)))

  (unless (implementation 'logger)
    (migrate (load-implementation 'logger) T T))

  (trigger 'startup)

  (l:info :radiance "Starting up.")
  (l:info :radiance "Ensuring prerequisites.")
  (unless (implementation 'server)
    (migrate (load-implementation 'server) T T))

  (l:info :radiance "Starting server.")
  (trigger 'server-start)

  (l:info :radiance "Migrating already loaded systems.")
  (dolist (system (asdf:already-loaded-systems))
    (let ((system (ensure-system system)))
      (when (typep system 'virtual-module)
        (migrate system T T))))
  
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
