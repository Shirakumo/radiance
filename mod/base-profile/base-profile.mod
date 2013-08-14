#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.base-profile
  (:nicknames :radiance-mod-base-profile)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-base-profile)

(defmodule base-profile ()
  "Provides standard user profile functions and panels."
  (:fullname "Base-Profile"
   :author "Nicolas Hafner"
   :version "0.0.1"
   :license "Artistic"
   :url "http://tymoon.eu"
   :implements '(profile))
  
  (:components ((:file "profile")
                (:file "settings")
                (:file "userpage"))
   :depends-on (:ironclad :drakma)))
