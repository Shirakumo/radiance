#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-cache
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-CACHE"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance cache interface implementation offering a convenient and simple disk-file-based caching mechanism."
  :components ((:file "cache"))
  :depends-on (:uiop))
