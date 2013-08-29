#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.trivial-menu
  (:nicknames :radiance-mod-trivial-menu)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-trivial-menu)

(defmodule trivial-menu ()
  "Provides standard cros-module menu."
  (:fullname "Trivial-Menu"
   :author "Nicolas Hafner"
   :version "0.0.1"
   :license "Artistic"
   :url "http://tymoon.eu"
   :dependencies '(admin)
   :implements '())
  
  (:components ((:file "menu")
                (:file "admin")))

  (menu-node :initarg :menu-node :initform NIL :accessor menu-node))
