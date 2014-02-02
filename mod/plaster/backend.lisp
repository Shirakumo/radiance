#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(admin:define-panel general plaster (:lquery (template "plaster/admin-general.html") :menu-icon "fa-file-text" :menu-tooltip "General plaster settings")
  (uibox:fill-foreach (dm:get "plaster-types" :all) "#template"))

(profile:define-panel preferences plaster (:lquery (template "plaster/user-preferences.html") :menu-icon "" :menu-tooltip "")
  )
