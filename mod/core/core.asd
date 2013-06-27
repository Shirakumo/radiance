#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.core.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.core.asdf)

(defsystem radiance-mod-core
  :name "Radiance Core"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Core system facilities for TyNETv5 Radiance."
  :long-description ""
  :components ((:file "package")
               (:file "modules" :depends-on ("package"))
               (:file "triggers" :depends-on ("package" "modules"))
               (:file "implements" :depends-on ("package" "modules"))
               (:file "flash-dispatch" :depends-on ("package" "implements" "modules"))
               (:file "mongo-database" :depends-on ("package" "implements"))
               (:file "kickstart-dispatch" :depends-on ("package" "implements" "modules" "triggers" "mongo-database")))
  :depends-on (:radiance-lib-core))
