#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-admin)


(admin::m-define-panel :trivial-admin index core (:menu-icon "fa-home" :menu-tooltip "Index" :lquery (template "admin/panel-index.html"))
  )

(admin::m-define-panel :trivial-admin modules core (:menu-icon "fa-tasks" :menu-tooltip "Manage radiance modules" :lquery (template "admin/panel-modules.html"))
  (uibox:fill-foreach (mapcar #'asdf:find-system *radiance-modules*) "tbody tr"))

(admin::m-define-panel :trivial-admin hooks core (:menu-icon "fa-random" :menu-tooltip "Manage triggers and hooks" :lquery (template "admin/panel-hooks.html"))
  (uibox:fill-foreach
   (loop for space being the hash-keys of *radiance-hooks*
         for hooks being the hash-values of *radiance-hooks*
         collect (list :space space :hooks (loop for hook being the hash-keys of hooks
                                                 for triggers being the hash-values of hooks
                                                 collect (list :hook hook :triggers triggers))))
   "#spaces .tablebox"))
