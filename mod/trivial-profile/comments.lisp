#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(define-hook (:server :init) (:documentation "Initialize the trivial-profile comment database.")
  (db:create "trivial-profile-comments" '(("user" :varchar 32) ("author" :varchar 32) ("time" :integer) ("text" :text)) :indices '("user")))

(define-api comment/add (text user) (:method :POST :access-branch "user.comment")
  (db:insert "trivial-profile-comments" `(("user" . ,user) ("author" . ,(user:field (user) "username")) ("time" . ,(get-unix-time)) ("text" . ,text)))
  (user:action (user) (format NIL "Commented on ~a's profile." user) :public T)
  (server:redirect (concatenate 'string "/" user)))

(define-api comment/delete () (:method :POST :access-branch "user.comment")
  (let ((selected (or (server:post "selected[]")
                      (list (server:post "id")))))
    (dolist (id selected)
      (with-model (model user author) ("trivial-profile-comments" (db:query (:= "_id" id)))
        (if (or (authorized-p "admin.profile.comments")
                (string= (user:field (user) "username") user)
                (string= (user:field (user) "username") author))
            (progn (user:action (user) (format NIL "Deleted comment #~a from ~a by ~a" id user author) :public NIL)
                   (dm:delete model)
                   (server:redirect (get-redirect)))
            (error 'api-auth-error :apicall 'comment/delete :text "You are not authorized to delete this comment."))))))
