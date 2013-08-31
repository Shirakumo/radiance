#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defmethod init-comments-db ((module trivial-profile))
  (db-create T "trivial-profile-comments" '(("user" :varchar 32) ("author" :varchar 32) ("time" :integer) ("text" :text)) :indices '("user")))
(defhook :server :init (get-module :trivial-profile) #'init-comments-db)

(defapi comment/add (text user) (:method :POST :access-branch "user.comment")
  (db-insert T "trivial-profile-comments" `(("user" . ,user) ("author" . ,(user-field (user) "username")) ("time" . ,(get-unix-time)) ("text" . ,text)))
  (user-action (user) (format NIL "Commented on ~a's profile." user) :public T)
  (redirect (concatenate 'string "/" user)))

(defapi comment/delete (id) (:method :POST :access-branch "user.comment")
  (with-model (model user author) ("trivial-profile-comments" (query (:= "_id" id)))
    (if (or (authorized-p "admin.profile.comments")
            (string= (user-field (user) "username") user)
            (string= (user-field (user) "username") author))
        (progn (user-action (user) (format NIL "Deleted comment #~a from ~a by ~a" id user author) :public NIL)
               (model-delete model)
               (redirect))
        (error 'api-auth-error :apicall 'comment/delete :text "You are not authorized to delete this comment."))))
