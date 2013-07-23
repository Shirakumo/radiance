#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmodule flash-dispatch (dispatcher)
  "Simple static lookup-table dispatcher"
  (:fullname "Flash Dispatcher" 
   :author "Nicolas Hafner" 
   :version "0.0.1"
   :license "Artistic" 
   :url "http://tymoon.eu"

   :implements '(dispatcher)
   :dependencies ())
   
  (:components ((:file "flash-dispatch")))
  (hooks :initform () :accessor hooks :allocation :class))
