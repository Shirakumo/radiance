#|
  This file is a part of TyNETv5/Radiance
  (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.db-introspect.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.db-introspect.asdf)

(defsystem db-introspect
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :name "db-introspect"
  :version "0.5.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A simple administration interface to interact with Radiance's database."
  :serial T
  :components ((:file "admin"))
  :depends-on ((:interface :data-model)
               (:interface :admin)
               :r-clip))
