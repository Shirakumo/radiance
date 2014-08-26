#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-auth
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-AUTH"
  :components ((:file "auth"))
  :depends-on ((:interface :session)
               (:interface :user)
               ;;(:interface :profile)
               :r-clip
               :crypto-shortcuts))
