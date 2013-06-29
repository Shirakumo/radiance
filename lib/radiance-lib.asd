#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.asdf)

(defsystem radiance-lib
  :name "TymoonNET v5 Radiance Runtime Libraries"
  :version "0.0.1"
  :license "LLGPL"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Sub-Build System for the Radiance Libraries."
  :long-description ""
  :components ()
  :depends-on (:lquery
               :radiance-lib-core))
