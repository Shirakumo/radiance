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
  :components ((:file "package")
               (:file "toolkit" :depends-on ("package"))
               (:file "module" :depends-on ("package"))
               (:file "trigger" :depends-on ("package" "module"))
               (:file "implement" :depends-on ("package" "module")))
  :depends-on (:cl-json))
