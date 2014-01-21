#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(declaim (inline continuation-handler))
(defun continuation-handler (request)
  (let* ((rcid (server:post-or-get "rcid"))
         (cont (when rcid
                 (ignore-errors (auth:authenticate))
                 (continuation rcid))))
    (if cont
        (with-accessors ((id id) (request request) (function continuation-function)) cont
          (v:debug :radiance.server.continuations "Resuming continuation ~a" cont)
          (let ((result (funcall function)))
            (remhash id (session:field *radiance-session* 'CONTINUATIONS))
            result))
        (dispatcher:dispatch request))))

(declaim (inline static-handler))
(defun file-handler (request)
  (if (and (> (length (path request)) 8)
           (string-equal (path request) "/static/" :end1 8))
      (progn (server:serve-file (static (subseq (path request) 0 8)))
             NIL)
      (continuation-handler request)))

(declaim (inline present-error))
(defun present-error (err &optional unexpected)
  (v:error :radiance.server.request "Encountered error: ~a" err)
  ($ (initialize (static "html/error/501.html")))
  ($ "#error h2" (text (format NIL "Error of type ~a" (class-name (class-of err)))))
  ($ "#error pre" (text (trivial-backtrace:print-backtrace err :output NIL)))
  (when unexpected
    ($ "h1" (text "Unexpected Internal Error"))
    ($ "head title" (text "Unexpected Internal Eerror"))
    ($ "html" (add-class :unexpected)))
  (server:set-response-content ($ (serialize) (node)))
  (invoke-restart 'skip-request))

(declaim (inline error-handler))
(defun error-handler (request)
  (handler-bind
      ((error-page #'(lambda (err)
                       (server:set-return-code (slot-value err 'code))
                       (server:set-response-content (read-data-file (format nil "static/html/error/~a.html" (slot-value err 'code))))
                       (invoke-restart 'skip-request)))
       (radiance-error #'present-error)
       (error #'(lambda (err) (present-error err T))))
    (with-simple-restart (skip-request "Skip the request and show the response stored in *radiance-request*.")
      (let ((result (file-handler request)))
        (typecase result
          (null)
          (string (server:set-response-content result))
          (list (server:set-response-content (concatenate-strings result))))
        (trigger :server :post-processing)
        result))))

(defun handler (request reply)
  "Propagates the call to the next handler registered in the implements."
  (let ((*radiance-request* request) (*radiance-response* reply) (*radiance-session* NIL))
    (v:info :radiance.server.request "~a ~a" (server:remote-address) request)
    (incf *radiance-request-total*)
    (incf *radiance-request-count*)
    
    (error-handler request)
    
    (decf *radiance-request-count*)))

(defun server-running-p ()
  (and server:*implementation*
       (server:get-listeners)))

(defun manage (action &key (config-file))
  "Manage the TymoonNETv5 web server."
  (etypecase action (symbol) (string))
  (setf action (find-symbol (string-upcase action) :radiance))
  (if config-file (setf *radiance-config-file* config-file))
  (funcall action))

(defun start ()
  "Loads the configuration and starts the TyNETv5 server."
  (if (server-running-p)
      (v:fatal :radiance.server.status "Server already running!")
      (progn
        (setf *radiance-startup-time* (get-unix-time))
        (v:info :radiance.server.status "Loading config...")
        (load-config)
        (when (string-equal (config :root) "autodetect") 
          (config :root (directory-namestring (asdf:system-source-directory :radiance))))

        (v:info :radiance.server.status "Loading modules...")
        (load-modules)
        (asdf:load-system "radiance-server")
        
        (v:info :radiance.server.status "Setting up server...")
        (server:set-default-content-type "application/xhtml+xml")
        (if (db:implementation)
            (progn
              (v:info :radiance.server.status "Connecting database...")
              (db:connect (config :database)))
            (v:warn :radiance.server.status "No database implementation defined!"))

        (v:info :radiance.server.status "Triggering INIT...")
        (trigger :server :init)

        (if (user:implementation)
            (user:action (user:get "sys") "INIT" :public T)
            (v:warn :radiance.server.status "No user implementation defined!"))

        (v:info :radiance.server.status "Starting listeners...")
        (mapcar #'(lambda (port) (server:start-listener (make-keyword (format NIL "PORT-~a" port)) :port port)) (config :ports))
        
        (v:info :radiance.server.status "Startup finished."))))  

(defun stop ()
  "Shuts down the TyNETv5 server."
  (if (server-running-p)
      (progn
        (v:info :radiance.server.status "Stopping listeners...")
        (dolist (listener (server:get-listeners))
          (server:stop-listener listener))
        
        (v:info :radiance.server.status "Triggering SHUTDOWN...")
        (trigger :server :shutdown)
        
        (when (user:implementation)
          (user:action (user:get "sys") "SHUTDOWN" :public T))

        (when (db:implementation)
          (v:info :radiance.server.status "Disconnecting Database...")
          (db:disconnect))

        (setf *radiance-request-count* 0
              *radiance-request-total* 0
              *radiance-startup-time* 0)

        (v:info :radiance.server.status "SHUTDOWN finished."))
      (v:fatal :radiance.server.status "Server isn't running!")))

(defun restart-server ()
  "Performs a stop, followed by a start."
  (stop)
  (start))

(defun status ()
  "Prints status information about the running server."
  (format T "Server running: ~:[No~*~*~*~;Yes~%Listeners: ~a~%Current requests: ~a~%Total requests: ~a~]"
          (server:get-listeners) (length (server:get-listeners)) *radiance-request-count* *radiance-request-total*))
