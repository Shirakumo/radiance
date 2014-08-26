#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-model
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-MODEL"
  :components ((:file "model"))
  :depends-on ((:interface :database)))
