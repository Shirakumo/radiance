#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defclass verify-user (user)
  ((username :initarg :name :initform (error "Username required.") :reader username)
   (model :initarg :model :initform () :reader model)))

(implement 'user (make-instance 'verify-user :name "SYS" :model (model-hull (implementation 'data-model) "verify-users")))

(defmethod print-object ((user verify-user) out)
  (print-unreadable-object (user out :type T)
    (format out "~a~:[ UNSAVED~;~]" (username user) (user-saved-p user))))

(defmethod user-get ((user verify-user) (username symbol) &key)
  "Retrieve a user from the db or create it if it is inexistent."
  (user-get user (string-downcase (symbol-name username))))

(defmethod user-get ((user verify-user) (username string) &key)
  "Retrieve a user from the db or create it if it is inexistent."
  (let ((model (model-get-one (implementation 'data-model) 
                              "verify-users" (query (= "username" username)))))
    (when (not model)
      (setf model (model-hull (implementation 'data-model) "verify-users"))
      (setf (model-field model "username") username))
    (make-instance 'verify-user :name username :model model)))

(defmethod user-field ((user verify-user) (field string) &key value)
  "Set or get a field of the user. Note that this will not save it to the database!"
  (if value
      (setf (model-field (model user) field) value)
      (model-field (model user) field)))

(defmethod user-save ((user verify-user) &key)
  "Saves the user to the database."
  (if (user-saved-p user)
      (model-save (model user))
      (model-insert (model user)))
  user)

(defmethod user-saved-p ((user verify-user) &key)
  "Returns T if the user is backed against the database."
  (not (model-hull-p (model user))))

(defmethod check ((user verify-user) &rest branch &key &allow-other-keys)
  "Check if the user has access to this permission branch. Returns the branch if permitted, otherwise NIL."
  (let ((perms (user-field user "perms")))
    (when perms
      (loop for line in (split-sequence:split-sequence #\newline perms)
         do (loop for leaf-a in branch
                 for leaf-b in (split-sequence:split-sequence #\. line)
                 do (cond
                      ((not (string= leaf-a leaf-b)) (return))
                      ((string= leaf-a "*") (return-from check branch))
                      ((string= leaf-b "*") (return-from check branch))
                      ((eq leaf-a (last branch)) (return-from check branch)))))))
  NIL)
