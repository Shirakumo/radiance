#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(admin:define-panel avatar profile (:access-branch "admin.profile.avatar" :menu-icon "fa-picture-o" :menu-tooltip "Change avatar settings" :lquery (template "trivial-profile/admin-avatar.html"))
  )

(admin:define-panel fields profile (:access-branch "admin.profile.fields" :menu-icon "fa-list" :menu-tooltip "Change user profile fields" :lquery (template "trivial-profile/admin-fields.html"))
  (uibox:fill-foreach (dm:get "trivial-profile-fields" :all :sort '(("field" . :DESC)) :limit -1) "tbody tr"))

(admin:define-panel comments profile (:access-branch "admin.profile.comments" :menu-icon "fa-comment" :menu-tooltip "Manage user profile comments" :lquery (template "trivial-profile/admin-comments.html"))
  (uibox:fill-foreach (dm:get "trivial-profile-comments" :all :sort '(("time" . :DESC)) :limit -1) "tbody tr"))

(define-api fields/add (field type value public) (:method :POST :access-branch "admin.profile.fields")
  (db:insert "trivial-profile-fields" `(("field" . ,field) ("type" . ,type) ("value" . ,value) ("public" . ,public)))
  (server:redirect "/profile/fields"))

(define-api fields/edit (orig-field field type value public) (:method :POST :access-branch "admin.profile.fields")
  (with-model model ("trivial-profile-fields" (db:query (:= "field" orig-field)) :save T)
    (setf (getdf model "field") field
          (getdf model "type") type
          (getdf model "value") value
          (getdf model "public") public))
  (server:redirect "/profile/fields"))

(define-api fields/delete () (:method :POST :access-branch "admin.profile.fields")
  (let ((selected (or (server:post "selected[]") (list (server:post "field")))))
    (dolist (field selected)
      (db:remove "trivial-profile-fields" (db:query (:= "field" field)) :limit NIL)))
  (server:redirect "/profile/fields"))
