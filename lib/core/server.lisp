#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-log-verbose* NIL "Whether to print a verbose log to stdout.")
(defvar *radiance-acceptors*   NIL "List of all Hunchentoot acceptors that run this server.")
(defvar *radiance-request-count* 0 "Counter for the current amount of requests being handled.")
(defvar *radiance-request-total* 0 "Counter for the total amount of requests handled.")
(defvar *radiance-handlers*    NIL "List of Hunchentoot handlers.")
(defvar *radiance-request*     NIL "Current request object.")
(defvar *radiance-session*     NIL "Current session object, if any,")

(defclass request (hunchentoot:request)
  ((response :initform hunchentoot:*reply* :initarg :response :accessor response)
   (subdomains :initform () :initarg :subdomains :accessor subdomains)
   (domain :initform (config :domain) :initarg :domain :accessor domain)
   (path :initform "/" :initarg :path :accessor path)
   (port :initform (config :port) :initarg :port :accessor port))
  (:documentation "Radiance request class."))

(defmethod print-object ((request request) out)
  (print-unreadable-object (request out :type T)
    (format out "~a:~a → (~a ~a)" (domain request) (port request) (subdomains request) (path request))))

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
        (log:info "Loading Config...")
        (load-config)
        (log:info "Loading implementations...")
        (discover-modules)
        (load-implementations)
        (log:info "Setting up Hunchentoot...")
        (let ((acceptors (loop for port in (config :ports) 
                            collect (make-instance 'hunchentoot:easy-acceptor :port port :request-class 'radiance:request))))
          (if (not *radiance-handlers*)
              (setf *radiance-handlers* 
                    (list (hunchentoot:create-folder-dispatcher-and-handler "/static/" (merge-pathnames "data/static/" (pathname (config :root))))
                          #'handler)))
          (setf hunchentoot:*dispatch-table* *radiance-handlers*)
          (setf hunchentoot:*default-content-type* "application/xhtml+xml")
          (log:info "Triggering INIT...")
          (trigger 'init)
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
        (trigger 'shutdown)
        (setf *radiance-request-count* 0)
        (setf *radiance-request-total* 0)
        (setf *radiance-acceptors* NIL)
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
  (let* ((path (hunchentoot:script-name request))
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
         (*radiance-session* NIL))

    (setf (subdomains request) subdomains
          (domain request) domain
          (path request) path
          (port request) port)
    (setf *radiance-request-total* (1+ *radiance-request-total*))
    (setf *radiance-request-count* (1+ *radiance-request-count*))
    (let ((result (dispatch (implementation 'dispatcher) request)))
      (cond ((stringp result) (setf (response request) result))
            ((listp result) (setf (response request) (concatenate-strings result)))))
    ;;(setf (response *radiance-request*) (format nil "~a ~a ~a" domains subdomains domain))
    (setf *radiance-request-count* (1- *radiance-request-count*))
    ;;(log:info "RESPONSE:  ~a" (response request))
    (lambda () (response request))))
