#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(define-admin-panel users verify (:access-branch "admin.verify.users.*" :menu-icon "icon-user" :menu-tooltip "View and manage user accounts" :lquery (template "verify/admin-users-overview.html"))
  (uibox:fill-foreach (model-get T "verify-users" :all :limit -1) "tbody tr"))

(define-admin-panel permissions verify (:access-branch "admin.verify.permissions.*" :menu-icon "icon-shield" :menu-tooltip "Change user permissions" :lquery (template "verify/admin-perms.html"))
  )

(define-admin-panel actions verify (:access-branch "admin.verify.actions.*" :menu-icon "icon-list-ul" :menu-tooltip "View a list of user actions" :lquery (template "verify/admin-actions.html"))
  (uibox:fill-foreach (model-get T "verify-actions" :all :limit -1) "tbody tr"))

(define-admin-panel sessions verify (:access-branch "admin.verify.sessions.*" :menu-icon "icon-signin" :menu-tooltip "See currently active sessions" :lquery (template "verify/admin-session.html"))
  (uibox:fill-foreach (alexandria:hash-table-values *verify-sessions*) "tbody tr"))

(define-admin-panel authentication verify (:access-branch "admin.verify.authentication.*" :menu-icon "icon-key" :menu-tooltip "Manage authentication mechanisms" :lquery (template "verify/admin-auth.html"))
  )
