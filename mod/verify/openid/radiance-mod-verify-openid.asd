#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(defpackage org.tymoonnext.radiance.mod.verify.openid.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.verify.openid.asdf)

(defsystem radiance-mod-verify-openid
  :name "OpenID Extension for Verify"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "OpenID Mechanism for Radiance's Verify"
  :long-description ""
  :components ((:file "openid"))
  :depends-on (:radiance-mod-core
               :radiance-mod-verify
               :cl-openid
               :puri))
