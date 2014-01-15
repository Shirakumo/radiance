#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.admin
  (:nicknames :radiance-mod-admin)
  (:use :cl :radiance :lquery)
  (:export ))
(in-package :radiance-mod-admin)

(asdf:defsystem trivial-admin
  :class :radiance-module
  :name "Administrator Interface"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :defsystem-depends-on (:radiance)
  :depends-on (:uibox :radiance-dispatcher :radiance-user)
  :implement ((:admin :trivial-admin))
  :components ((:file "admin")))
