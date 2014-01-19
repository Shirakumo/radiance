#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-admin)


(admin:define-panel index core (:menu-icon "icon-home" :menu-tooltip "Index" :lquery (template "admin/panel-index.html"))
  )

(admin:define-panel modules core (:menu-icon "icon-tasks" :menu-tooltip "Manage radiance modules" :lquery (template "admin/panel-modules.html"))
  (uibox:fill-foreach (alexandria:hash-table-values *radiance-modules*) "tbody tr"))

(admin:define-panel hooks core (:menu-icon "icon-random" :menu-tooltip "Manage triggers and hooks" :lquery (template "admin/panel-hooks.html"))
  (uibox:fill-foreach
   (loop for space being the hash-keys of *radiance-hooks*
         for hooks being the hash-values of *radiance-hooks*
         collect (list :space space :hooks (loop for hook being the hash-keys of hooks
                                                 for triggers being the hash-values of hooks
                                                 collect (list :hook hook :triggers triggers))))
   "#spaces .tablebox"))
