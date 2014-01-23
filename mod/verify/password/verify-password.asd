#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.password
  (:nicknames :radiance-mod-verify-password)
  (:use :cl :radiance :lquery :radiance-mod-verify)
  (:export))
(in-package :radiance-mod-verify-password)

(asdf:defsystem verify-password
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "Password Mechanism for Verify" 
  :author "Nicolas Hafner" 
  :version "0.0.1"
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :components ((:file "password"))
  :depends-on (:verify
               :uibox
               :radiance-auth
               :radiance-session
               :radiance-user))
