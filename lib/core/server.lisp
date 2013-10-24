#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defun manage (action &key (config-file) (verbose T))
  "Manage the TymoonNETv5 web server."
  (if (not (or (stringp action) (functionp action) (symbolp action)))
      (error "Action must be a function, symbol or string."))
  (if (stringp action)
      (setf action (find-symbol (string-upcase action) :radiance)))
  (if (symbolp action)
      (setf action (symbol-function action)))
  (if (not action) 
      (error "Requested action not found."))
  
  (setf *radiance-log-verbose* verbose)
  (if config-file (setf *radiance-config-file* config-file))
  (funcall action))

(defun start ()
  "Loads the configuration and starts the TyNETv5 server."
  (if (server-running-p)
      (log:fatal :radiance.server.status "Server already running!")
      (progn
        (setf *radiance-startup-time* (get-unix-time))
        (log:info :radiance.server.status "Loading Config...")
        (load-config)
        (if (string-equal (config :root) "autodetect") 
            (config :root (format nil "~a" (asdf:system-source-directory :radiance))))
        (log:info :radiance.server.status "Loading implementations...")
        (discover-modules)
        (load-implementations)
        (log:info :radiance.server.status "Setting up Hunchentoot...")
        (let ((acceptors (loop for port in (config :ports) 
                            collect (make-instance 'hunchentoot:easy-acceptor 
                                                   :port port
                                                   :access-log-destination NIL
                                                   :message-log-destination NIL
                                                   :request-class 'radiance:request))))
          (setf *radiance-handlers* 
                (list (hunchentoot:create-folder-dispatcher-and-handler "/static/" (merge-pathnames "data/static/" (pathname (config :root))))
                      (function handler)))
          (setf hunchentoot:*dispatch-table* *radiance-handlers*)
          (setf hunchentoot:*default-content-type* "application/xhtml+xml")
          (log:info :radiance.server.status "Connecting Database...")
          (db-connect T (config :database))
          (log:info :radiance.server.status "Triggering INIT...")
          (trigger :server :init)
          (user-action (user-get T "sys") "INIT" :public T)
          
          (loop for acceptor in acceptors
               do (progn (log:info :radiance.server.status "Starting acceptor ~a" acceptor)
                         (hunchentoot:start acceptor)))
          (setf *radiance-acceptors* acceptors)
          (log:info :radiance.server.status "INIT finished.")))))  

(defun stop ()
  "Shuts down the TyNETv5 server."
  (if (server-running-p)
      (progn
        (loop for acceptor in *radiance-acceptors*
             do (progn (log:info :radiance.server.status "Stopping acceptor ~a" acceptor)
                       (hunchentoot:stop acceptor)))
        (setf *radiance-acceptors* NIL)
        
        (log:info :radiance.server.status "Triggering SHUTDOWN...")
        (trigger :server :shutdown)
        (user-action (user-get T "sys") "SHUTDOWN" :public T)
        (log:info :radiance.server.status "Disconnecting Database...")
        (db-disconnect T)
        (setf *radiance-request-count* 0)
        (setf *radiance-request-total* 0)
        (setf *radiance-startup-time* 0)
        (log:info :radiance.server.status "SHUTDOWN finished."))
      (log:fatal :radiance.server.status "Server isn't running!")))

(defun restart ()
  "Performs a stop, followed by a start."
  (stop)
  (start))

(defun status ()
  "Prints status information about the running server."
  (format T "Server running: ~:[No~;Yes~]~%Acceptors: ~a~%Current requests: ~a~%Total requests: ~a"
          *radiance-acceptors* (length *radiance-acceptors*) *radiance-request-count* *radiance-request-total*))

(defun server-running-p ()
  (if *radiance-acceptors* T NIL))

(defun handler (&optional (request hunchentoot:*request*) (reply hunchentoot:*reply*))
  "Propagates the call to the next handler registered in the implements."
  (declare (optimize (speed 3) (safety 0)))
  (setf *last-ht-request* request)
  (setf *last-ht-reply* reply)
  (let ((*radiance-request* request) (*radiance-reply* reply) (*radiance-session* NIL))
    (parse-request request)
    (log:info :radiance.server.request "~a ~a" (remote-address) request)
    (incf *radiance-request-total*)
    (incf *radiance-request-count*)
    (let ((result (error-handler request)))
      (cond ((stringp result) (setf (response request) result))
            ((and result (listp result)) (setf (response request) (concatenate-strings result)))))
    (decf *radiance-request-count*)
    (lambda () (response request))))

(defun present-error (err &optional unexpected)
  (log:error :radiance.server.request "Encountered error: ~a" err)
  ($ (initialize (static "html/error/501.html")))
  ($ "#error h2" (text (format NIL "Error of type ~a" (class-name (class-of err)))))
  ($ "#error pre" (text (trivial-backtrace:print-backtrace err :output NIL)))
  (when unexpected
    ($ "h1" (text "Unexpected Internal Error"))
    ($ "head title" (text "Unexpected Internal Eerror"))
    ($ "html" (add-class :unexpected)))
  (setf (response *radiance-request*) ($ (serialize) (node)))
  (invoke-restart 'skip-request))

(defun error-handler (request)
  (handler-bind
      ((error-page #'(lambda (err)
                       (setf (hunchentoot:return-code* *radiance-reply*) (slot-value err 'code)
                             (response *radiance-request*) (read-data-file (format nil "static/html/error/~a.html" (slot-value err 'code))))
                       (invoke-restart 'skip-request)))
       (radiance-error #'present-error)
       (error #'(lambda (err) (present-error err T))))
    (with-simple-restart (skip-request "Skip the request and show the response stored in *radiance-request*.")
      (let* ((result (continuation-handler request))
             (post-result (trigger :server :post-processing result)))
        (cond ((stringp post-result) post-result)
              ((and post-result (listp post-result)) (concatenate-strings post-result))
              (T result))))))

(defun continuation-handler (request)
  (let* ((rcid (post-or-get-var "rcid"))
         (cont (when rcid
                 (ignore-errors (authenticate T))
                 (get-continuation rcid))))
    (if cont
        (with-accessors ((id id) (request request) (function continuation-function)) cont
          (let ((result (funcall function)))
            (remhash id (session-field *radiance-session* 'CONTINUATIONS))
            result))
        (dispatch T request))))
