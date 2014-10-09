#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.asdf)

(defsystem radiance
  :name "TymoonNET v5 Radiance"
  :version "2.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Version 5 of TyNET, an extensible framework library and multi-application CMS."
  :depends-on (:radiance-core
               :radiance-web))
