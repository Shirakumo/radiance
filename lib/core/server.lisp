#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass request (hunchentoot:request radiance:uri)
  ((response :initform NIL :initarg :response :accessor response))
  (:documentation "Radiance request class."))

(defmethod print-object ((request request) out)
  (print-unreadable-object (request out :type T)
    (format out "~a:~a → (~a /~a)" (domain request) (port request) (subdomains request) (path request))))

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
      (log:fatal "Server already running!")
      (progn
        (setf *radiance-startup-time* (get-unix-time))
        (log:info "Loading Config...")
        (load-config)
        (if (string-equal (config :root) "autodetect") 
            (config :root (format nil "~a" (asdf:system-source-directory :radiance))))
        (log:info "Loading implementations...")
        (discover-modules)
        (load-implementations)
        (log:info "Setting up Hunchentoot...")
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
          (log:info "Triggering INIT...")
          (trigger :server 'init)
          (loop for acceptor in acceptors
               do (progn (log:info "Starting acceptor ~a" acceptor)
                         (hunchentoot:start acceptor)))
          (setf *radiance-acceptors* acceptors)
          (log:info "INIT finished.")))))  

(defun stop ()
  "Shuts down the TyNETv5 server."
  (if (server-running-p)
      (progn
        (loop for acceptor in *radiance-acceptors*
             do (progn (log:info "Stopping acceptor ~a" acceptor)
                       (hunchentoot:stop acceptor)))
        (log:info "Triggering SHUTDOWN...")
        (trigger :server 'shutdown)
        (setf *radiance-request-count* 0)
        (setf *radiance-request-total* 0)
        (setf *radiance-acceptors* NIL)
        (setf *radiance-startup-time* 0)
        (log:info "SHUTDOWN finished."))
      (log:fatal "Server isn't running!")))

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

(defun handler (&optional (request hunchentoot:*request*))
  "Propagates the call to the next handler registered in the implements."
  (multiple-value-bind (result realtime runtime)
      (time-spent
        (let* ((path (string-left-trim "/" (hunchentoot:script-name request)))
               (host (hunchentoot:host request))
               (port (if (find #\: host)
                         (parse-integer (subseq host (1+ (search ":" host))))
                         80))
               (domains (split-sequence:split-sequence #\. (if (find #\: host)
                                                               (subseq host 0 (search ":" host))
                                                               host)))
               (subdomains (reverse (if (> (length domains) 2) (subseq domains 0 (- (length domains) 2)))))
               (domain (concatenate-strings (subseq domains (length subdomains)) "."))
               (*radiance-request* request)
               (*radiance-reply* hunchentoot:*reply*)
               (*radiance-session* NIL))
          
          (setf (subdomains request) subdomains
                (domain request) domain
                (path request) path
                (port request) port)
          (log:debug "REQUEST: ~a" request)
          (setf *radiance-request-total* (1+ *radiance-request-total*))
          (setf *radiance-request-count* (1+ *radiance-request-count*))
          (let ((result (dispatch (implementation 'dispatcher) request)))
        (cond ((stringp result) (setf (response request) result))
              ((listp result) (setf (response request) (concatenate-strings result)))))
          (setf *radiance-request-count* (1- *radiance-request-count*))
          (lambda () (response request))))
    (log:trace "TIME SPENT: ~fs real ~fs run" realtime runtime)
    result))
