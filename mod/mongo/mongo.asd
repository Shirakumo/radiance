#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-mongo
  (:use :cl :radiance)
  (:export :mongodb :mongo-data-model))
(in-package :radiance-mod-mongo)

(asdf:defsystem mongodb
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "MongoDB Binding" 
  :author "Nicolas Hafner" 
  :version "0.0.1" 
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :implement ((:database :mongodb)
              (:data-model :mongodb))
  :components ((:file "database")
               (:file "data-model" :depends-on ("database")))
  :depends-on (:cl-mongo))
