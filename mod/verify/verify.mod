#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify
  (:nicknames :radiance-mod-verify)
  (:use :cl :radiance :lquery)
  (:export :verify-auth 
           :verify-user 
           :verify-session 
           :defmechanism 
           :show-login 
           :show-register 
           :handle-login 
           :handle-register 
           :handle-link
           :auth-login-error
           :auth-register-error))
(in-package :radiance-mod-verify)

(let ((user-collection (make-collection 'user :access-mode "755" :description "Public user data"
                                        :columns '((id "555") (username "555") (displayname "755") (secret "700")))))
  
  (defmodule verify (auth)
    "Verification Module to provide user, session and authentication parts."
    (:fullname "Verify Authentication System" 
     :author "Nicolas Hafner" 
     :version "0.0.1"
     :license "Artistic" 
     :url "http://tymoon.eu"
               
     :collections (list user-collection)
     :dependencies '(data-model dispatcher)
     :implements '(user session auth)
     :asdf-system "radiance-mod-verify")
  
    (:components ((:file "crypto")
                  (:file "user")
                  (:file "auth" :depends-on ("user" "crypto"))
                  (:file "session" :depends-on ("user"))
                  (:file "sites" :depends-on ("auth" "session")))
     :depends-on (:split-sequence
                  :ironclad
                  :uuid))))
