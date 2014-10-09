#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:i-postmodern
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "A bridging library to allow using Postmodern as a Radiance database."
  :components ((:file "module")
               (:file "toolkit")
               (:file "connection")
               (:file "query")
               (:file "database"))
  :depends-on (:postmodern
               :bordeaux-threads))
