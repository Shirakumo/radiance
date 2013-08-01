#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defclass verify-user (user)
  ((username :initarg :name :initform (error "Username required.") :reader username)
   (model :initarg :model :initform () :reader model)))

(implement 'user (make-instance 'verify-user :name "SYS" :model (model-hull T "verify-users")))

(defmethod print-object ((user verify-user) out)
  (print-unreadable-object (user out :type T)
    (format out "~a~:[ UNSAVED~;~]" (username user) (user-saved-p user))))

(defmethod user-get ((user verify-user) (username symbol) &key)
  "Retrieve a user from the db or create it if it is inexistent."
  (user-get user (string-downcase (symbol-name username))))

(defmethod user-get ((user verify-user) (username string) &key)
  "Retrieve a user from the db or create it if it is inexistent."
  (setf username (string-downcase username))
  (let ((model (model-get-one T "verify-users" (:= "username" username))))
    (when (not model)
      (setf model (model-hull T "verify-users"))
      (setf (model-field model "username") username))
    (make-instance 'verify-user :name username :model model)))

(defmethod user-field ((user verify-user) (field string) &key (value NIL v-p))
  "Set or get a field of the user. Note that this will not save it to the database!"
  (if v-p
      (setf (model-field (model user) field) value)
      (model-field (model user) field)))

(defun set-user-field (user field value)
  (user-field user field :value value))
(defsetf user-field set-user-field)

(defmethod user-save ((user verify-user) &key)
  "Saves the user to the database."
  (if (user-saved-p user)
      (model-save (model user))
      (model-insert (model user)))
  user)

(defmethod user-saved-p ((user verify-user) &key)
  "Returns T if the user is backed against the database."
  (not (model-hull-p (model user))))

(defmethod user-check ((user verify-user) branch &key &allow-other-keys)
  "Check if the user has access to this permission branch. Returns the branch if permitted, otherwise NIL."
  (let ((perms (user-field user "perms"))
        (branch (split-sequence:split-sequence #\. branch)))
    (when perms
      (loop for line in (split-sequence:split-sequence #\newline perms)
         do (loop for leaf-a in branch
               for leaf-b in (split-sequence:split-sequence #\. line)
               do (cond
                    ((string= leaf-a "*") (return-from user-check branch))
                    ((string= leaf-b "*") (return-from user-check branch))
                    ((not (string= leaf-a leaf-b)) (return)))
               finally (if (string= leaf-a leaf-b) (return-from user-check branch))))))
  NIL)

(defmethod user-grant ((user verify-user) branch &key &allow-other-keys)
  "Grants a new permission branch to the users permissions."
  (setf (user-field user "perms")
        (concatenate 'string 
                     (user-field user "perms")
                     (format nil "~%") branch)))

(defmethod user-prohibit ((user verify-user) branch &key &allow-other-keys)
  "Remove permission from the user's permissions matching this branch."
  (let ((perms (user-field user "perms"))
        (branch (split-sequence:split-sequence #\. branch))
        (to-remove ()))
    (when perms
      (setf perms (split-sequence:split-sequence #\newline perms))
      (loop for line in perms
         do (loop for leaf-a in branch
               for leaf-b in (split-sequence:split-sequence #\. line)
               do (cond 
                    ((string= leaf-a "*") (nappend to-remove (list line)) (return))
                    ((string= leaf-b "*") (nappend to-remove (list line)) (return))
                    ((not (string= leaf-a leaf-b)) (return)))
               finally (if (string= leaf-a leaf-b) (nappend to-remove (list line)))))
      (setf (user-field user "perms")
            (concatenate-strings (remove-if (lambda (item) (find item to-remove :test #'string=)) perms) #\newline)))))
