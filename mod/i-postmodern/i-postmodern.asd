#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:cl-user)
(asdf:defsystem #:i-postmodern
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "module")
               (:file "toolkit")
               (:file "connection")
               (:file "query")
               (:file "database"))
  :depends-on (:postmodern
               :bordeaux-threads))
