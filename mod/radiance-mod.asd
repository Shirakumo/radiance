#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.asdf)

(defsystem radiance-mod
  :name "TymoonNET v5 Radiance Modules"
  :version "0.0.1"
  :license "Lisp GPL"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Sub-Build System for the Radiance Modules.."
  :long-description ""
  :components (())
  :depends-on (:radiance-mod-core))
