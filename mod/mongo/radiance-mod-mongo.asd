#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#


(defpackage org.tymoonnext.radiance.mod.mongo.asdf
  (:use :cl :asdf))
(in-package :org.tymoonnext.radiance.mod.mongo.asdf)

(defsystem radiance-mod-mongo
  :name "Radiance MongoDB Module"
  :version "0.0.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Database implementation for MongoDB."
  :long-description ""
  :components ((:file "database")
               (:file "data-model" :depends-on ("database")))
  :depends-on (:cl-mongo
               :radiance-mod-core))
