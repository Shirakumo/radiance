#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.admin
  (:nicknames :radiance-mod-admin)
  (:use :cl :radiance :lquery :alexandria))
(in-package :radiance-mod-admin)

(asdf:defsystem trivial-admin
  :class :radiance-module
  :name "Administrator Interface"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :defsystem-depends-on (:radiance)
  :depends-on (:uibox :radiance-core :alexandria)
  :implement ((:admin :trivial-admin))
  :serial T
  :components ((:file "admin")
               (:file "default-panels")))
