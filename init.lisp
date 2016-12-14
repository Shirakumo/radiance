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

;; Handle default server startup.
;; FIXME: Not sure if this is the right place for this.
(define-trigger (server-start 'launch-listeners) ()
  (defaulted-config '(((:port 8080))) :server :instances)
  (dotimes (i (length (config :server :instances)))
    (unless (find (format NIL "~a:~a"
                          (config :server :instances i :address)
                          (config :server :instances i :port))
                  (server:listeners)
                  :test #'string-equal)
      (server:start (config :server :instances i :port)
                    :address (config :server :instances i :address)
                    :ssl-key (config :server :instances i :ssl-key)
                    :ssl-cert (config :server :instances i :ssl-cert)
                    :ssl-pass (config :server :instances i :ssl-pass)))))

(define-trigger (server-stop 'stop-listeners) ()
  (loop for name in (server:listeners)
        do (let* ((pos (position #\: name))
                  (port (parse-integer (subseq name (1+ pos))))
                  (address (subseq name 0 pos)))
             (server:stop port (unless (string= address "NIL") address)))))
