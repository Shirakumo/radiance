#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-sysinfo
  (:use :cl :radiance :lquery))

(in-package :radiance-mod-sysinfo)

(defmodule sysinfo ()
  "Displays system info."
  (:fullname "System Info" 
   :author "Nicolas Hafner" 
   :version "0.0.1" 
   :license "Artistic" 
   :url "http://tymoon.eu")
   
  (:components ((:file "site"))
   :depends-on (:closer-mop)))
