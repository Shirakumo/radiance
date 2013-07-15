#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defvar *verify-sessions* (make-hash-table :test 'equal) "Session registry")

(defclass verify-session (session)
  ((username :initarg :name :initform (error "Username required.") :reader username)
   (fields :initarg :fields :initform (make-hash-table :test 'equal) :reader fields)
   (active :initarg :active :initform T :accessor active)))

(implement 'session (make-instance 'verify-session :name "NOOP"))

(defmethod print-object ((session verify-session) out)
  (print-unreadable-object (session out :type T)
    (format out "~a" (username session))))

(defmethod session-field ((session verify-session) (field string) &key value &allow-other-keys)
  "Set or get a field of the user. Note that this will not save it to the database!"
  (if value
      (setf (gethash field (fields session)) value)
      (gethash field (fields session))))

(defmethod session-start ((session verify-session) (user user) &key &allow-other-keys)
  "Starts a new session for the given user, enters it in the registry and returns the session object."
  (let ((session (make-instance 'verify-session :name (user-field user "username"))))
    (setf (gethash (username session) *verify-sessions*) session)))

(defmethod session-end ((session verify-session) &key &allow-other-keys)
  "Ends the session and removes it from the registry."
  (remhash (username session) *verify-sessions*)
  (setf (active session) NIL)
  session)

(defmethod session-user ((session verify-session) &key &allow-other-keys)
  "Retrieves a database instance of the corresponding user object."
  (user-get (implementation 'user) (username session)))

(defmethod session-active-p ((session verify-session) &key &allow-other-keys)
  "Returns T if the session is still active, otherwise NIL."
  (active session))
