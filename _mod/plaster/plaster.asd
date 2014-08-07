#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-plaster
  (:use :cl :radiance :lquery :alexandria))
(in-package :radiance-mod-plaster)

(asdf:defsystem plaster
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "Plaster Pasting Service" 
  :author "Nicolas Hafner"
  :version "0.0.1" 
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :implement ()
  :components ((:file "frontend")
               (:file "backend")
               (:file "pastebin-type-map")
               (:file "api"))
  :depends-on (:radiance-database
               :radiance-data-model
               :radiance-user
               :radiance-session
               :radiance-admin
               :radiance-profile
               :uibox
               :alexandria
               :radiance-crypto
               :drakma
               :cl-ppcre))
