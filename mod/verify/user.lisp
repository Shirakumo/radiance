#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(define-hook (:server :init) (:documentation "Initialize verify user database.")
  (db:create "verify-users" '(("username" :varchar 32) ("displayname" :varchar 32) ("secret" :varchar 16) ("email" :varchar 64) ("register-date" :integer) ("perms" :text)) :indices '("username"))
  (db:create "verify-actions" '(("username" :varchar 32) ("action" :text) ("public" :varchar 3) ("time" :integer)) :indices '("username")))

(defclass verify-user (user:class)
  ((username :initarg :name :initform (error "Username required.") :reader username)
   (model :initarg :model :initform () :reader model)))

(defmethod print-object ((user verify-user) out)
  (print-unreadable-object (user out :type T)
    (format out "~a~:[ UNSAVED~;~]" (username user) (user:saved-p user)))
  user)

(define-interface-method user:get ((username symbol))
  (user:get (string-downcase (symbol-name username))))

(define-interface-method user:get ((username string))
  (%user-get (string-downcase username)))  

(defun %user-get (username)
  (let ((model (dm:get-one "verify-users" (db:query (:= "username" username)))))
    (when (not model)
      (setf model (dm:hull "verify-users"))
      (setf (getdf model "username") username))
    (make-instance 'verify-user :name username :model model)))

(define-interface-method user:field ((user verify-user) (field string) &key (value NIL v-p))
  (if v-p
      (setf (getdf (model user) field) value)
      (dm:field (model user) field)))

(define-interface-method user:action ((user verify-user) action &key public)
  (db:insert "verify-actions" `(("username" . ,(username user)) ("action" . ,action) ("public" . ,(format NIL "~a" public)) ("time" . ,(get-unix-time)))))

(define-interface-method user:get-actions ((user verify-user) n &key public oldest-first)
  (let ((query (if public 
                   (db:query (:= "username" (username user)) (:= "public" "T"))
                   (db:query (:= "username" (username user))))))
    (db:iterate "verify-actions" query
                #'(lambda (column) (list (assoc "action" column :test #'string=)
                                         (assoc "time" column :test #'string=)))
                :sort `(("time" . ,(if oldest-first :ASC :DESC))) :limit n :skip 0)))

(define-interface-method user:save ((user verify-user))
  (if (user:saved-p user)
      (dm:save (model user))
      (dm:insert (model user)))
  user)

(define-interface-method user:saved-p ((user verify-user))
  (not (dm:hull-p (model user))))

(define-interface-method user:check ((user verify-user) branch)
  (v:trace :verify.user "Checking permissions of ~a for ~a" user branch)
  (block user-check
    (let ((perms (user:field user "perms"))
          (branch (split-sequence:split-sequence #\. branch)))
      (when perms
        (loop for line in (split-sequence:split-sequence #\newline perms)
              do (loop for leaf-a in branch
                       for leaf-b in (split-sequence:split-sequence #\. line)
                       do (cond
                            ((string= leaf-a "*") (return-from user-check branch))
                            ((string= leaf-b "*") (return-from user-check branch))
                            ((not (string= leaf-a leaf-b)) (return)))
                       finally (if (string= leaf-a leaf-b) (return-from user-check branch)))))))
  NIL)

(define-interface-method user:grant ((user verify-user) branch)
  (v:debug :verify.user "Granting permissions for ~a: ~a" user branch)
  (setf (getdf user "perms")
        (format nil "~:[~;~:*~a~%~]~a" (user:field user "perms") branch)))

(define-interface-method user:prohibit ((user verify-user) branch)
  (let ((perms (user:field user "perms"))
        (branch (split-sequence:split-sequence #\. branch))
        (to-remove ()))
    (v:debug :verify.user "Revoking permissions for ~a: ~a" user branch)
    (when perms
      (setf perms (split-sequence:split-sequence #\newline perms))
      (loop for line in perms
         do (loop for leaf-a in branch
               for leaf-b in (split-sequence:split-sequence #\. line)
               do (cond 
                    ((string= leaf-a "*") (appendf to-remove (list line)) (return))
                    ((string= leaf-b "*") (appendf to-remove (list line)) (return))
                    ((not (string= leaf-a leaf-b)) (return)))
               finally (if (string= leaf-a leaf-b) (appendf to-remove (list line)))))
      (setf (getdf user "perms")
            (concatenate-strings (remove-if (lambda (item) (find item to-remove :test #'string=)) perms) #\newline)))))
