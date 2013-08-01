#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sysinfo)

(defpage debug #u"inf./debug" ()
   (when (hunchentoot:get-parameter "eval")
     (setf (hunchentoot:content-type* *radiance-reply*) "text/plain; format=utf-8")
     (eval (read-from-string (hunchentoot:get-parameter "eval")))))
