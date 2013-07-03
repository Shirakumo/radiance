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
  :components ((:file "server")
               (:file "interfaces")
               (:file "flash-dispatch" :depends-on ("interfaces"))
               ;(:file "kickstart-dispatch" :depends-on ("interfaces"))
               )
  :depends-on (:hunchentoot
               :split-sequence
               :alexandria
               :radiance-lib-core))
