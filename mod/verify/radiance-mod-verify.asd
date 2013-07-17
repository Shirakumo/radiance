#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(defpackage org.tymoonnext.radiance.mod.verify.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.verify.asdf)

(defsystem radiance-mod-verify
  :name "Radiance Core Verify System"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Core verification system for Radiance."
  :long-description ""
  :components ((:file "crypto")
               (:file "user")
               (:file "auth" :depends-on ("user" "crypto"))
               (:file "session" :depends-on ("user"))
               (:file "mechanisms" :depends-on ("auth" "crypto"))
               (:file "sites" :depends-on ("auth" "session")))
  :depends-on (:radiance-mod-core
               :split-sequence
               :ironclad
               :uuid))
