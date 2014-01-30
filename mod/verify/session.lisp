#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defvar *verify-sessions* (make-hash-table :test 'equal) "Session registry")

(defclass verify-session (session:class)
  ((uuid :initarg :uuid :initform (format nil "~a" (uuid:make-v4-uuid)) :reader uuid)
   (time :initarg :time :initform (get-unix-time) :reader session-time)
   (user :initarg :user :initform (error "User required") :accessor s-user)
   (fields :initarg :fields :initform (make-hash-table :test 'equal) :reader fields)
   (remote :initarg :remote :initform NIL :reader remote)
   (active :initarg :active :initform T :accessor active)))

(defmethod print-object ((session verify-session) out)
  (print-unreadable-object (session out :type T)
    (format out "~a" (s-user session)))
  session)

(define-interface-method session:get ((uuid string))
  "Returns the requested session instance if applicable."
  (gethash uuid *verify-sessions*))

(define-interface-method session:get-all ()
  "Returns all sessions still on the server."
  (alexandria:hash-table-values *verify-sessions*))

(define-interface-method session:start ((user user:class))
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (let ((session (make-instance 'verify-session :user user)))
    (v:debug :verify.session "Starting new session for ~a with UUID ~a" user (uuid session))
    (when *radiance-request*
        (setf (slot-value session 'remote) (server:remote-address))
        (server:set-cookie "token" :value (make-session-cookie session))
        (setf *radiance-session* session))
    (setf (gethash (uuid session) *verify-sessions*) session)))

(define-interface-method session:start ((username string))
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (session:start (user:get username)))

(define-interface-method session:start-temp ()
  "Starts a new temporary session, enters it in the registry and returns the session object."
  (session:start "temp"))

(define-interface-method session:end ((session verify-session))
  "Ends the session and removes it from the registry."
  (v:debug :verify.session "Terminating session for ~a with UUID ~a" (s-user session) (uuid session))
  (remhash (uuid session) *verify-sessions*)
  (setf (active session) NIL)
  (if (eq *radiance-session* session) (server:set-cookie "token"))
  session)

(define-interface-method session:field ((session verify-session) field &key (value NIL v-p))
  "Set or get a field of the user. Note that this will not save it to the database!"
  (if v-p
      (setf (gethash field (fields session)) value)
      (gethash field (fields session))))

(define-interface-method session:uuid ((session verify-session))
  "Returns the UUID of the session instance."
  (uuid session))

(define-interface-method session:user ((session verify-session))
  "Retrieves a database instance of the corresponding user object."
  (s-user session))

(define-interface-method session:active-p ((session verify-session))
  "Returns T if the session is still active, otherwise NIL."
  (active session))

(define-interface-method session:temp-p ((session verify-session))
  "Returns T if the session is only temporary, otherwise NIL."
  (equal (username (s-user session)) "temp"))

(defun make-session-cookie (session)
  "Create the encrypted session cookie data. Does not actually set the cookie!"
  (let* ((username (if (s-user session) (username (s-user session)) "temp"))
         (timestamp (get-unix-time))
         (random (make-random-string))
         (uuid (uuid session))
         (data (format NIL "~a:~a:~a" timestamp random uuid)))
    (if (config-tree :verify :session :use-per-user-secret)
        (setf data (rad-crypto:encrypt data (user:field (s-user session) "secret"))))
    (rad-crypto:encrypt (format NIL "~a-~a" username data) (config-tree :verify :session :secret))))
