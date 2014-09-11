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
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A simple Radiance authentication interface implementation offering basic password logins."
  :components ((:file "auth"))
  :depends-on ((:interface :session)
               (:interface :user)
               :r-clip
               :crypto-shortcuts))
