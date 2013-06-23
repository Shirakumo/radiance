#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.TEMPLATE.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.TEMPLATE.asdf)

(defsystem radiance-lib-TEMPLATE
  :name "Radiance TEMPLATE Library"
  :version "0.0.1"
  :license "LLGPL"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "TEMPLATE library for TyNETv5 Radiance."
  :long-description ""
  :components ((:file "TEMPLATE"))
  :depends-on ())
