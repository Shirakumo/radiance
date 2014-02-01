#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(admin:define-panel general plaster (:lquery (tempalte "plaster/admin-general.html") :menu-icon "" :menu-tooltip "")
  )

(profile:define-panel preferences plaster (:lquery (template "plaster/preferences.html") :menu-icon "" :menu-tooltip "")
  )
