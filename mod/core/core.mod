#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmodule radiance-core (core)
  "Core module prividing module scanning and stuff."
  (:fullname "TyNETv5 Radiance"
   :author "Nicolas Hafner"
   :version "0.0.1"
   :license "Artistic"
   :url "http://tymoon.eu"
   
   :implements '(core)
   :asdf-system :radiance-mod-core))
