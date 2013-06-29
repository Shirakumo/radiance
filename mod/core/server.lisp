#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-log-verbose* NIL "Whether to print a verbose log to stdout.")
(defvar *radiance-acceptor*    NIL "The main acceptor used in Hunchentoot. Initialized at start.")
(defvar *radiance-request-count* 0 "Counter for the current amount of requests being handled.")

(defclass request ()
  ((request :initform (error "Hunchentoot request required.") :initarg :request :accessor request)
   (response :initform () :initarg :response :accessor response)
   (subdomains :initform () :initarg :subdomains :accessor subdomains)
   (domain :initform (config :domain) :initarg :domain :accessor host)
   (path :initform "/" :initarg :path :accessor path)
   (port :initform (config :port) :initarg :port :accessor port))
  (:documentation "Radiance request class."))

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
  (format T "Loading Config...~%")
  (load-config)
  (format T "Setting up Hunchentoot...~%")
  (let ((acceptor (make-instance 'hunchentoot:easy-acceptor :port (config :port))))
    (setf hunchentoot:*dispatch-table* (list #'handler))
    (format T "Triggering INIT...~%")
    (trigger 'init)
    (format T "Starting Hunchentoot...~%")
    (hunchentoot:start acceptor)
    (setf *radiance-acceptor* acceptor)))
    

(defun stop ()
  "Shuts down the TyNETv5 server."
  (format T "Stopping Hunchentoot...~%")
  (hunchentoot:stop *radiance-acceptor*)
  (format T "Triggering SHUTDOWN...~%")
  (trigger 'shutdown)
  (setf *radiance-acceptor* NIL))

(defun restart ()
  "Performs a stop, followed by a start."
  (stop)
  (start))

(defun status ()
  "Prints status information about the running server."
  )

(defun handler (&optional (request hunchentoot:*request*))
  "Propagates the call to the next handler registered in the implements."
  (let* ((path (hunchentoot:script-name request))
         (host (hunchentoot:host request))
         (port (if (find #\: host)
                   (parse-integer (subseq host (1+ (search ":" host))))
                   80))
         (domains (split-sequence:split-sequence #\. (if (= port 80)
                                                         host
                                                         (subseq host 0 (find ":" host)))))
         (subdomains (if (> 2 (length domains)) (subseq domains 0 (- (length domains) 2))))
         (domain (concatenate-strings (subseq domains (length subdomains)) ".")))
    (let ((*radiance-request* (make-instance 'request
                                             :request request
                                             :subdomains subdomains
                                             :domain domain
                                             :path path
                                             :port port)))
      (setf *radiance-request-count* (1+ *radiance-request-count*))
      ;;(dispatch (implementation 'dispatcher) *radiance-request*)
      (setf (response *radiance-request*) "おはよう、これはＬＩＳＰにＴＹＮＥＴです！<br />Hi, this is TyNET in Lisp!")
      (setf *radiance-request-count* (1- *radiance-request-count*))
      
      (hunchentoot:log-message* 100 "~a" *radiance-request*)
      (lambda () (response *radiance-request*)))))
