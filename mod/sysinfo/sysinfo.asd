#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-sysinfo
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-sysinfo)

(asdf:defsystem sysinfo
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "System Info" 
  :author "Nicolas Hafner" 
  :version "0.0.1" 
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :components ((:file "sysinfo") (:file "debug"))
  :depends-on (:closer-mop :radiance-dispatcher :lquery :hunchentoot))
