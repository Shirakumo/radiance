#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defvar *verify-sessions* (make-hash-table :test 'equal) "Session registry")

(defclass verify-session (session)
  ((uuid :initarg :uuid :initform (format nil "~a" (uuid:make-v4-uuid)) :reader uuid)
   (time :initarg :time :initform (get-unix-time) :reader session-time)
   (user :initarg :user :initform (error "User required") :accessor s-user)
   (fields :initarg :fields :initform (make-hash-table :test 'equal) :reader fields)
   (remote :initarg :remote :initform NIL :reader remote)
   (active :initarg :active :initform T :accessor active)))

(defmethod print-object ((session verify-session) out)
  (print-unreadable-object (session out :type T)
    (format out "~a" (s-user session))))

(implement 'session (make-instance 'verify-session :uuid NIL :time NIL :user NIL :fields NIL :active NIL :remote NIL))

(defmethod session-get ((session verify-session) (uuid string) &key &allow-other-keys)
  "Returns the requested session instance if applicable."
  (gethash uuid *verify-sessions*))

(defmethod session-get-all ((session verify-session) &key)
  "Returns all sessions still on the server."
  (alexandria:hash-table-values *verify-sessions*))

(defmethod session-start ((session verify-session) (user user) &key &allow-other-keys)
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (let ((session (make-instance 'verify-session :user user)))
    (v:debug :verify.session "Starting new session for ~a with UUID ~a" user (uuid session))
    (when *radiance-request*
        (setf (slot-value session 'remote) (remote-address))
        (set-cookie "token" :value (make-session-cookie session))
        (setf *radiance-session* session))
    (setf (gethash (uuid session) *verify-sessions*) session)))

(defmethod session-start ((session verify-session) (username string) &key &allow-other-keys)
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (session-start session (user-get T username)))

(defmethod session-start-temp ((session verify-session) &key &allow-other-keys)
  "Starts a new temporary session, enters it in the registry and returns the session object."
  (session-start session "temp"))

(defmethod session-end ((session verify-session) &key &allow-other-keys)
  "Ends the session and removes it from the registry."
  (v:debug :verify.session "Terminating session for ~a with UUID ~a" (s-user session) (uuid session))
  (remhash (uuid session) *verify-sessions*)
  (setf (active session) NIL)
  (if (eq *radiance-session* session) (set-cookie "token"))
  session)

(defmethod session-field ((session verify-session) field &key (value NIL v-p) &allow-other-keys)
  "Set or get a field of the user. Note that this will not save it to the database!"
  (if v-p
      (setf (gethash field (fields session)) value)
      (gethash field (fields session))))

(defmethod session-uuid ((session verify-session) &key &allow-other-keys)
  "Returns the UUID of the session instance."
  (uuid session))

(defmethod session-user ((session verify-session) &key &allow-other-keys)
  "Retrieves a database instance of the corresponding user object."
  (s-user session))

(defmethod session-active-p ((session verify-session) &key &allow-other-keys)
  "Returns T if the session is still active, otherwise NIL."
  (active session))

(defmethod session-temp-p ((session verify-session) &key &allow-other-keys)
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
        (setf data (encrypt data (user-field (s-user session) "secret"))))
    (encrypt (format NIL "~a-~a" username data) (config-tree :verify :session :secret))))
