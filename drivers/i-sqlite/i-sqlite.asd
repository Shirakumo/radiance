#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:i-sqlite
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.0"
  :description "A bridging library to allow using SQLite3 as a Radiance database."
  :components ((:file "module")
               (:file "load")
               (:file "extension")
               (:file "connection")
               (:file "query")
               (:file "toolkit")
               (:file "database"))
  :depends-on (:cffi))
