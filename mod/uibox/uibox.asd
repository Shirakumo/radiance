#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.uibox
  (:nicknames :radiance-mod-uibox :uibox)
  (:use :cl :radiance :lquery :alexandria)
  (:export
   :fill-node
   :fill-all
   :fill-foreach
   :define-fill-function
   :parse-data
   :input-select
   :notice
   :confirm))
(in-package :radiance-mod-uibox)

(asdf:defsystem uibox
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "UIBox"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"  
  :components ((:file "fill")
               (:file "build"))
  :depends-on (:string-case :alexandria))
