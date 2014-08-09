#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:i-hunchentoot
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "i-hunchentoot"))
  :depends-on (:hunchentoot :cl-ppcre))
