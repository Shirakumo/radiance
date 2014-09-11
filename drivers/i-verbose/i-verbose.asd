#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:i-verbose
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A briding library to allow using Verbose as Radiance's logging implementation."
  :components ((:file "i-verbose"))
  :depends-on (:verbose))
