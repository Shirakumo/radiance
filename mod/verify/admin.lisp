#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

;(defmacro defadmin (name category (&key module (modulevar (gensym "MODULE-")) lquery access-branch menu-icon menu-tooltip) &body body)

(define-admin-panel users verify (:access-branch "admin.verify.users.*" :menu-icon "icon-user" :menu-tooltip "View and manage user accounts")
  )

(define-admin-panel permissions verify (:access-branch "admin.verify.permissions.*" :menu-icon "icon-shield" :menu-tooltip "Change user permissions")
  )

(define-admin-panel actions verify (:access-branch "admin.verify.actions.*" :menu-icon "icon-list-ul" :menu-tooltip "View a list of user actions")
  )

(define-admin-panel sessions verify (:access-branch "admin.verify.sessions.*" :menu-icon "icon-signin" :menu-tooltip "See currently active sessions")
  )

(define-admin-panel authentication verify (:access-branch "admin.verify.authentication.*" :menu-icon "icon-key" :menu-tooltip "Manage authentication mechanisms")
  )
