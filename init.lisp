#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance
  (:nicknames :radiance :tynet-5 :tynet)
  (:use :cl :hunchentoot :cl-json :radiance-mod-core)
  (:export :manage))

(in-package :radiance)

;; Startup function
(defvar *RADIANCE-CONFIG-FILE* NIL "Radiance's main JSON configuration file.")
(defvar *RADIANCE-CONFIG*      NIL "Radiance's main static configuration.")
(defvar *RADIANCE-LOG-VERBOSE* NIL "Whether to print a verbose log to stdout.")
(defvar *RADIANCE-ACCEPTOR*    NIL "The main acceptor used in Hunchentoot. Initialized at start.")

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
  (setf *radiance-config-file* config-file)
  (funcall action))

(defun start ()
  "Loads the configuration and starts the TyNETv5 server."
  (load-config)
  (setf *radiance-acceptor* (make-instance 'hunchentoot:easy-acceptor :port (cdr (assoc :port *radiance-config*))))
  (setf huchentoot:*dispatch-table* (list (hunchentoot:create-regex-dispatcher ".*" 'handler)))
  (hunchentoot:start *radiance-acceptor*)
  ;(trigger 'init)
  )

(defun stop ()
  "Shuts down the TyNETv5 server."
  (hunchentoot:stop *radiance-acceptor*)
  ;(trigger 'shutdown)
  )

(defun restart ()
  "Performs a stop, followed by a start."
  (stop)
  (start))

(defun status ()
  "Prints status information about the running server."
  )

(defun load-config (&optional (config-file *radiance-config-file*))
  "(Re)load the static configuration"
  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *radiance-config* (json:decode-json file))))

(defun handler ()
  "Propagates the call to the next handler registered in the implements."
  ;(trigger 'request (module 'load (implements 'dispatch)))
  )
