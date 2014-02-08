#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(asdf:defsystem trivial-core
  :class :radiance-module
  :name "Radiance Core" 
  :author "Nicolas Hafner" 
  :version "5.0.1"
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  
  :defsystem-depends-on (:radiance)
  :implement ((:core :trivial-core))
  :depends-on ()
  :components ((:file "page")
               (:file "api")
               (:file "defaults")))
