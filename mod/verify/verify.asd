#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify
  (:nicknames :radiance-mod-verify)
  (:use :cl :radiance :lquery :alexandria)
  (:export :username
           :verify-auth 
           :verify-user 
           :verify-session
           :defmechanism 
           :show-login 
           :show-register
           :show-options
           :handle-register
           :page-login
           :page-register
           :auth-login-error
           :auth-register-error))
(in-package :radiance-mod-verify)

(asdf:defsystem verify
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "Verify Authentication System" 
  :author "Nicolas Hafner" 
  :version "0.0.1"
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :serial T
  :components ((:file "user")
               (:file "session")
               (:file "auth")
               (:file "sites")
               (:file "admin")
               (:file "api"))
  :depends-on (:split-sequence
               :radiance-crypto
               :uuid
               :radiance-database
               :radiance-data-model
               :radiance-dispatcher
               :radiance-admin
               :uibox)
  :implement ((:user :verify)
              (:session :verify)
              (:auth :verify)))
