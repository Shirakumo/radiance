#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.openid
  (:nicknames :radiance-mod-verify-openid)
  (:use :cl :radiance :lquery :radiance-mod-verify)
  (:export))
(in-package :radiance-mod-verify-openid)

(asdf:defsystem verify-openid
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "OpenID Mechanism for Verify" 
  :author "Nicolas Hafner" 
  :version "0.0.1"
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :components ((:file "openid"))
  :depends-on (:verify
               :cl-openid
               :puri))
