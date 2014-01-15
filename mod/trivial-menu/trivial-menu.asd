#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.trivial-menu
  (:nicknames :radiance-mod-trivial-menu)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-trivial-menu)

(asdf:defsystem trivial-menu
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "Trivial-Menu"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :components ((:file "menu")
               (:file "admin"))
  :depends-on (:string-case :radiance-admin))
