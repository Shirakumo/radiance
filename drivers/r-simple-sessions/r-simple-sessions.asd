#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-sessions
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-SESSIONS"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :version "1.0.1"
  :description "An implementation for Radiance's sessions interface using encrypted cookies for session tracking."
  :components ((:file "session"))
  :depends-on (:crypto-shortcuts
               :local-time
               :uuid))
