#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.core.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.lib.core.asdf)

(defsystem radiance-lib-core
  :name "Radiance Core Libraries"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Core libraries for TyNETv5 Radiance."
  :long-description ""
  :serial T
  :components ((:file "package")
               (:file "globals")
               (:file "module")
               (:file "trigger")
               (:file "implement")
               (:file "toolkit")
               (:file "interfaces")
               (:file "site")
               (:file "server"))
  :depends-on (:cl-json
               :hunchentoot
	       :log4cl
               :uuid
               :cl-fad
               :lquery
               :cl-ppcre))
