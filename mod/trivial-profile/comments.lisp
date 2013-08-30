#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defmethod init-comments-db ((module trivial-profile))
  (db-create T "trivial-profile-comments" '(("user" :varchar 32) ("author" :varchar 32) ("time" :integer) ("text" :text)) :indices '("user")))
(defhook :server :init (get-module :trivial-profile) #'init-comments-db)

(defapi comment (text user) (:method :POST :access-branch "user.comment")
  (db-insert T "trivial-profile-comments" `(("user" . ,user) ("author" . ,(user-field (user) "username")) ("time" . ,(get-unix-time)) ("text" . ,text)))
  (redirect (concatenate 'string "/" user)))
