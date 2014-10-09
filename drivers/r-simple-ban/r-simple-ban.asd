#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-ban
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-BAN"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "Radiance ban interface implementation offering a simple banning mechanism."
  :components ((:file "ban"))
  :depends-on ())
