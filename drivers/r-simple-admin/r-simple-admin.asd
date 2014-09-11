#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-admin
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-ADMIN"
  :serial T
  :components ((:file "admin")
               (:file "panels"))
  :depends-on ((:interface :auth)
               :r-clip
               :alexandria))
