#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sysinfo)

(define-page debug #u"inf./debug" ()
  (when (server:get "eval")
    (server:set-content-type "text/plain; format=utf-8")
    (eval (read-from-string (server:get "eval")))))

(define-page data-file #u"inf./data-file" ()
  (when (server:get "path")
    (read-data-file (server:get "path"))))
