#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.radiance.web.asdf
  (:use #:cl #:asdf))
(in-package :org.tymoonnext.radiance.lib.radiance.web.asdf)

(defsystem radiance-web
  :class "radiance:module"
  :defsystem-depends-on (:radiance-core)
  :name "Radiance-Web"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Radiance framework web component."
  :long-description ""
  :serial T
  :components ((:file "module")
               (:file "uri")
               (:file "routing")
               (:file "dispatch")
               (:file "request")
               (:file "continuation")
               (:file "interfaces")
               (:file "toolkit")
               (:file "conditions")
               (:file "options")
               (:file "page")
               (:file "api")
               (:file "defaults")
               (:file "init"))
  :depends-on (:cl-ppcre
               :bordeaux-threads
               :trivial-mimes
               :local-time))
