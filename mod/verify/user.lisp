#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defmethod init-user-db ((module verify))
  (db-create T "verify-users" '(("username" :varchar 32) ("displayname" :varchar 32) ("secret" :varchar 16) ("email" :varchar 64) ("register-date" :integer) ("perms" :text)) :indices '("username"))
  (db-create T "verify-actions" '(("username" :varchar 32) ("action" :text) ("public" :varchar 3) ("time" :integer)) :indices '("username")))
(defhook :server :init (get-module :verify) #'init-user-db)

(defclass verify-user (user)
  ((username :initarg :name :initform (error "Username required.") :reader username)
   (model :initarg :model :initform () :reader model)))

(implement 'user (make-instance 'verify-user :name "SYS" :model (model-hull T "verify-users")))

(defmethod print-object ((user verify-user) out)
  (print-unreadable-object (user out :type T)
    (format out "~a~:[ UNSAVED~;~]" (username user) (user-saved-p user))))

(defmethod user-get ((user verify-user) (username symbol) &key)
  (user-get user (string-downcase (symbol-name username))))

(defmethod user-get ((user verify-user) (username string) &key)
  (setf username (string-downcase username))
  (if (null (request-field :users)) (setf (request-field :users) (make-hash-table :test 'equal)))
  (or (gethash username (request-field :users))
      (let ((model (model-get-one T "verify-users" (query (:= "username" username)))))
        (when (not model)
          (setf model (model-hull T "verify-users"))
          (setf (model-field model "username") username))
        (setf (gethash username (request-field :users))
              (make-instance 'verify-user :name username :model model)))))

(defmethod user-field ((user verify-user) (field string) &key (value NIL v-p))
  (if v-p
      (setf (model-field (model user) field) value)
      (model-field (model user) field)))

(defmethod user-action ((user verify-user) action &key public)
  (db-insert T "verify-actions" `(("username" . ,(username user)) ("action" . ,action) ("public" . ,(format NIL "~a" public)) ("time" . ,(get-unix-time)))))

(defmethod user-get-actions ((user verify-user) n &key public oldest-first)
  (let ((query (if public 
                   (query (:= "username" (username user)) (:= "public" "T"))
                   (query (:= "username" (username user))))))
    (db-iterate T "verify-actions" query
                #'(lambda (column) (list (assoc "action" column :test #'string=)
                                         (assoc "time" column :test #'string=)))
                :sort `(("time" . ,(if oldest-first :ASC :DESC))) :limit n :skip 0)))

(defun set-user-field (user field value)
  (user-field user field :value value))
(defsetf user-field set-user-field)

(defmethod user-save ((user verify-user) &key)
  (if (user-saved-p user)
      (model-save (model user))
      (model-insert (model user)))
  user)

(defmethod user-saved-p ((user verify-user) &key)
  (not (model-hull-p (model user))))

(defmethod user-check ((user verify-user) branch &key &allow-other-keys)
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
  (setf (user-field user "perms")
        (format nil "~:[~;~:*~a~%~]~a" (user-field user "perms") branch)))

(defmethod user-prohibit ((user verify-user) branch &key &allow-other-keys)
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
