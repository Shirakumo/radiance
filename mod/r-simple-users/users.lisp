#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module simple-users
  (:use #:cl #:radiance)
  (:implements #:user))
(in-package #:simple-users)

(defvar *user-cache* (make-hash-table :test 'equalp))

(define-trigger db:connected ()
  (db:create 'simple-users '((username (:varchar 32)) (permissions :text)) :indices '(username))
  (db:create 'simple-users-fields '((uid :integer) (field (:varchar 64)) (value :text)) :indices '(uid))
  (db:create 'simple-users-actions '((uid :integer) (time :integer) (public (:integer 1)) (action :text)) :indices '(uid))
  (user::sync))

(defclass user (user:user)
  ((username :initarg :username :initform (error "USERNAME required.") :accessor username)
   (id :initarg :id :initform (error "ID required.") :accessor id)
   (fields :initarg :fields :initform (make-hash-table :test 'equalp) :accessor fields)
   (permissions :initarg :permissions :initform () :accessor permissions)
   (modified :initarg :modified :initform () :accessor modified)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream)
    (format stream "USER ~a~:[~; *~]" (username user) (modified user))))

(defmethod initialize-instance :after ((user user) &key)
  (setf (gethash (username user) *user-cache*) user))

(defun user:get (username &key (if-does-not-exist NIL))
  (let ((username (string-downcase username)))
    (or (gethash username *user-cache*)
        (ecase if-does-not-exist
          (:create
           (make-instance 'user
                          :username username
                          :id (db:insert 'simple-users `((username . ,username) (permissions . "")))))
          (:error (error 'user-not-found :user username))
          (:anonymous (user:get "anonymous"))
          ((NIL :NIL))))))

(defun user:username (user)
  (username user))

(defun user:field (user field)
  (gethash field (fields user)))

(defun (setf user:field) (value user field)
  (push (cons field (null (gethash field (fields user)))) (modified user))
  (setf (gethash field (fields user)) value))

(defun user:save (user)
  (loop for (name . insert) = (pop (modified user))
        while name
        do (if insert
               (db:insert 'simple-users-fields `((uid . ,(id user)) (field . ,name) (value . ,(gethash name (fields user)))))
               (db:update 'simple-users-fields (db:query (:and (:= 'uid (id user)) (:= 'field name))) `((value . ,(gethash name (fields user)))))))
  user)

(defun user:saved-p (user)
  (not (modified user)))

(defun user:remove (user)
  (trigger 'user:remove user)
  (db:remove 'simple-users-actions (db:query (:= 'uid (id user))))
  (db:remove 'simple-users-fields (db:query (:= 'uid (id user))))
  (db:remove 'simple-users (db:query (:= '_id (id user))))
  (setf (fields user) NIL
        (id user) NIL
        (permissions user) NIL
        (modified user) NIL)
  user)

(defun save-perms (user)
  (db:update 'simple-users (db:query (:= '_id (id user)))
             `((permissions . ,(format NIL "~{~{~a~^.~}~^~%~}" (permissions user))))))

(defun branch-matches (permission branch)
  (when (<= (length permission) (length branch))
    (loop for leaf-a in permission
          for leaf-b in branch
          always (string-equal leaf-a leaf-b))))

(defun ensure-branch (branch)
  (etypecase branch
    (string (cl-ppcre:split "\\." branch))
    (list branch)))

(defun user:check (user branch)
  (let ((branch (ensure-branch branch)))
    (loop for perm in (permissions user)
          thereis (branch-matches perm branch))))

(defun user:grant (user branch)
  (let ((branch (ensure-branch branch)))
    (push branch (permissions user))
    (save-perms user))
  user)

(defun user:prohibit (user branch)
  (let ((branch (ensure-branch branch)))
    (setf (permissions user)
          (remove-if #'(lambda (perm) (branch-matches perm branch)) (permissions user)))
    (save-perms user))
  user)

(defun user:action (user action public)
  (db:insert 'simple-users-actions `((uid . ,(id user)) (time . ,(get-universal-time)) (public . ,(if public 1 0)) (action . ,action)))
  (trigger 'user:action user action public)
  user)

(defun user:actions (user n &key (public T) oldest-first)
  (db:iterate 'simple-users-actions (if public
                                        (db:query (:and (:= 'uid (id user)) (:= 'public 1)))
                                        (db:query (:and (:= 'uid (id user)))))
              #'(lambda (ta) (gethash "action" ta))
              :fields '(action) :amount n :sort `((time ,(if oldest-first :ASC :DESC))) :accumulate T))

(defun user::sync ()
  (setf *user-cache* (make-hash-table :test 'equalp))
  (let ((idtable (make-hash-table :test 'eql)))
    (dolist (model (dm:get 'simple-users (db:query :all)))
      (l:debug :users "Loading ~a" (dm:field model "username"))
      (setf (gethash (dm:id model) idtable)
            (make-instance 'user
                           :id (dm:id model) :username (dm:field model "username")
                           :permissions (mapcar #'(lambda (b) (cl-ppcre:split "\\." b))
                                                (cl-ppcre:split "\\n" (dm:field model "permissions"))))))
    ;; sync fields
    (dolist (entry (dm:get 'simple-users-fields (db:query :all)))
      (let ((field (dm:field entry "field"))
            (value (dm:field entry "value"))
            (uid (dm:field entry "uid")))
        (l:debug :users "Set field ~a of ~a to ~s" field (gethash uid idtable) value)
        (setf (gethash field (fields (gethash uid idtable))) value)))
    ;; ensure anonymous user
    (user:get :anonymous :if-does-not-exist :create)
    (l:info :users "Synchronized ~d users from database." (hash-table-count idtable))))

(defmethod field ((user user) field)
  (gethash field (fields user)))

(defmethod (setf field) (value (user user) field)
  (setf (gethash field (fields user)) value))
