#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(defpackage org.tymoonnext.radiance.mod.verify.oauth.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.verify.oauth.asdf)

(defsystem radiance-mod-verify-oauth
  :name "OAuth Extension for Verify"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "OAuth Mechanism for Radiance's Verify"
  :long-description ""
  :components ((:file "oauth"))
  :depends-on (:radiance-mod-core
               :radiance-mod-verify
               :cl-oauth
               :cl-json))
