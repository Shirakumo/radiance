#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:simple-admin)

(admin:define-panel overview admin (:icon "fa-home")
  (r-clip:process
   (plump:parse (template "overview.ctml"))))
