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
   (user :initarg :user :initform (error "User required.") :reader user)
   (fields :initarg :fields :initform (make-hash-table :test 'equal) :reader fields)
   (active :initarg :active :initform T :accessor active)))

(defmethod print-object ((session verify-session) out)
  (print-unreadable-object (session out :type T)
    (format out "~a" (user session))))

(implement 'session (make-instance 'verify-session :uuid NIL :user NIL :fields NIL :active NIL))

(defmethod session-get ((session verify-session) (uuid string) &key &allow-other-keys)
  "Returns the requested session instance if applicable."
  (gethash uuid *verify-sessions*))

(defmethod session-start ((session verify-session) (user user) &key &allow-other-keys)
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (let ((session (make-instance 'verify-session :user user)))
    (log:debug "Starting new session for ~a with UUID ~a" user (uuid session))
    (setf (gethash (uuid session) *verify-sessions*) session)))

(defmethod session-start ((session verify-session) (username string) &key &allow-other-keys)
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (session-start session (user-get (implementation 'user) username)))

(defmethod session-end ((session verify-session) &key &allow-other-keys)
  "Ends the session and removes it from the registry."
  (log:debug "Terminating session for ~a with UUID ~a" (user session) (uuid session))
  (remhash (uuid session) *verify-sessions*)
  (setf (active session) NIL)
  session)

(defmethod session-field ((session verify-session) (field string) &key value &allow-other-keys)
  "Set or get a field of the user. Note that this will not save it to the database!"
  (if value
      (setf (gethash field (fields session)) value)
      (gethash field (fields session))))

(defmethod session-uuid ((session verify-session) &key &allow-other-keys)
  "Returns the UUID of the session instance."
  (uuid session))

(defmethod session-user ((session verify-session) &key &allow-other-keys)
  "Retrieves a database instance of the corresponding user object."
  (user session))

(defmethod session-active-p ((session verify-session) &key &allow-other-keys)
  "Returns T if the session is still active, otherwise NIL."
  (active session))
