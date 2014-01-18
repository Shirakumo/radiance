#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.trivial-profile
  (:nicknames :radiance-mod-trivial-profile)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-trivial-profile)

(asdf:defsystem trivial-profile
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "Trivial-Profile"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :serial T
  :components ((:file "gravatar")
               (:file "profile")
               (:file "comments")
               (:file "settings")
               (:file "userpage")
               (:file "admin"))
  :depends-on (:radiance-admin :radiance-parser :radiance-user :uibox :ironclad :drakma :cl-ppcre)
  :implement ((:profile :trivial-profile)))
