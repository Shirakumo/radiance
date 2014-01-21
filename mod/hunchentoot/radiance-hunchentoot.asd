#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defpackage :org.tymoonnext.radiance.mod.hunchentoot
  (:use :cl :radiance)
  (:nicknames :radiance-mod-hunchentoot))
(in-package :radiance-mod-hunchentoot)

(asdf:defsystem radiance-hunchentoot
  :class :radiance-module
  :name "Radiance Hunchentoot Server Interface" 
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"

  :defsystem-depends-on (:radiance)
  :implement ((:server :radiance-hunchentoot))
  :depends-on (:hunchentoot :alexandria)
  :serial T
  :components ((:file "request")
               (:file "server")))
