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
