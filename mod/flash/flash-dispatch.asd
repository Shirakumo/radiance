#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defpackage :org.tymoonnext.radiance.mod.flash-dispatch
  (:use :cl :radiance)
  (:nicknames :radiance-mod-flash-dispatch)
  (:export :flash-dispatch))

(in-package :radiance-mod-flash-dispatch)

(asdf:defsystem flash-dispatch
  :class module
  :name "Flash Dispatcher" 
  :author "Nicolas Hafner" 
  :version "0.0.1"
  :license "Artistic" 
  :homepage "http://tymoon.eu"

  :implement ((:dispatcher flash-dispatch))
  :components ((:file "flash-dispatch")))
