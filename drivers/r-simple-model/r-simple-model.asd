#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-model
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-MODEL"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "Trivially simple Radiance data-model interface implementation."
  :components ((:file "model"))
  :depends-on ((:interface :database)))
