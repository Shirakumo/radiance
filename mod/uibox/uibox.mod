#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.uibox
  (:nicknames :radiance-mod-uibox :uibox)
  (:use :cl :radiance :lquery :string-case)
  (:export :fill-node :fill-foreach))
(in-package :radiance-mod-uibox)

(defmodule uibox ()
  "Utility collection to build interfaces in HTML."
  (:fullname "UIBox"
   :author "Nicolas Hafner"
   :version "0.0.1"
   :license "Artistic"
   :url "http://tymoon.eu"
   :implements '(admin))
  
  (:components ((:file "fill"))
   :depends-on (:string-case)))
