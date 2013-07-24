#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sysinfo)

(defmethod site-debug ((sysinfo sysinfo))
  
  (if (hunchentoot:get-parameter "eval")
      (eval (read-from-string (hunchentoot:get-parameter "eval")))))

(defhook :sysinfo-debug (get-module 'sysinfo) #'site-debug)
(register (implementation 'dispatcher) :sysinfo-debug :subdomain "debug")
