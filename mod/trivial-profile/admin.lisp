#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(define-admin-panel avatar profile (:access-branch "admin.profile.avatar" :menu-icon "icon-picture" :menu-tooltip "Change avatar settings" :lquery (template "trivial-profile/admin-avatar.html"))
  )

(define-admin-panel fields profile (:access-branch "admin.profile.fields" :menu-icon "icon-list" :menu-tooltip "Change user profile fields" :lquery (template "trivial-profile/admin-fields.html"))
  (uibox:fill-foreach (model-get T "trivial-profile-fields" :all :sort '(("field" . :DESC)) :limit -1) "tbody tr"))

(define-admin-panel comments profile (:access-branch "admin.profile.comments" :menu-icon "icon-comment" :menu-tooltip "Manage user profile comments" :lquery (template "trivial-profile/admin-comments.html"))
  (uibox:fill-foreach (model-get T "trivial-profile-comments" :all :sort '(("time" . :DESC)) :limit -1) "tbody tr"))

(defapi fields/add (field type value public) (:method :POST :access-branch "admin.profile.fields")
  (db-insert T "trivial-profile-fields" `(("field" . ,field) ("type" . ,type) ("value" . ,value) ("public" . ,public)))
  (redirect "/profile/fields"))

(defapi fields/edit (orig-field field type value public) (:method :POST :access-branch "admin.profile.fields")
  (with-model model ("trivial-profile-fields" (query (:= "field" orig-field)) :save T)
    (setf (model-field model "field") field
          (model-field model "type") type
          (model-field model "value") value
          (model-field model "public") public))
  (redirect "/profile/fields"))

(defapi fields/delete () (:method :POST :access-branch "admin.profile.fields")
  (let ((selected (or (post-var "selected[]") (list (post-var "field")))))
    (dolist (field selected)
      (db-remove T "trivial-profile-fields" (query (:= "field" field)) :limit NIL)))
  (redirect "/profile/fields"))
