#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-rate
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-RATE"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance rate interface implementation offering a convenient db-backed rate limiting."
  :components ((:file "rate"))
  :depends-on ((:interface :database)
               (:interface :data-model)))
