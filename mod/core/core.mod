#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmodule core ()
  "Radiance Core Module, Mostly used for API."
  (:fullname "Radiance Core" 
   :author "Nicolas Hafner" 
   :version "5.0.1"
   :license "Artistic" 
   :url "http://tymoon.eu"

   :implements '(core)
   :dependencies '(dispatcher))

  (:components ((:file "api"))))
