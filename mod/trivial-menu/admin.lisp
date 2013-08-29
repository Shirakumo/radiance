#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-menu)

(define-admin-panel items menu (:access-branch "admin.menu.*" :menu-icon "icon-external-link-sign" :menu-tooltip "Change menu items." :lquery (template "trivial-menu/admin.html"))
  (uibox:fill-foreach (model-get T "trivial-menu" :all :sort '(("sort" . :ASC)) :limit -1) "tbody tr"))
