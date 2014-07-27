#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.radiance.core.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.radiance.lib.radiance.core.asdf)

(defsystem radiance-core
  :class "modularize:module"
  :defsystem-depends-on (:modularize) 
  :name "Radiance-Core"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Radiance framework core component."
  :long-description ""
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "interfaces"))
  :depends-on (:modularize-hooks
               :modularize-interfaces
               :universal-config))
