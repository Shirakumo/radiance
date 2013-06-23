#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.TEMPLATE.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.TEMPLATE.asdf)

(defsystem radiance-mod-TEMPLATE
  :name "Radiance TEMPLATE"
  :version "0.0.1"
  :license "GPL3"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "TEMPLATE for TyNETv5 Radiance."
  :long-description ""
  :components ((:file "TEMPLATE"))
  :depends-on (:radiance-lib-core))
