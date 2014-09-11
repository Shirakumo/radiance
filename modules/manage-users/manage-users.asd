#|
  This file is a part of TyNETv5/Radiance
  (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage #:org.tymoonnext.manage-users.asdf
  (:use #:cl #:asdf))
(in-package #:org.tymoonnext.manage-users.asdf)

(defsystem manage-users
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :name "manage-users"
  :version "0.5.1"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Radiance administration interface to allow user management."
  :serial T
  :components ((:file "admin"))
  :depends-on ((:interface :user)
               (:interface :admin)
               :r-clip))
