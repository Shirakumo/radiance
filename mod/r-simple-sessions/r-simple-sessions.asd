#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:r-simple-sessions
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :module-name "SIMPLE-SESSIONS"
  :components ((:file "session"))
  :depends-on (:crypto-shortcuts
               :local-time
               :uuid))
