#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.db-introspect
  (:nicknames :radiance-mod-db-introspect)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-db-introspect)

(asdf:defsystem db-introspect
  :class :radiance-module
  :name "Database Introspect"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :defsystem-depends-on (:radiance)
  :depends-on (:radiance-admin :uibox :string-case)
  :components ((:file "admin")))
