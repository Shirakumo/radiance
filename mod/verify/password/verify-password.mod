#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.password
  (:nicknames :radiance-mod-verify-password)
  (:use :cl :radiance :lquery :radiance-mod-verify)
  (:export))
(in-package :radiance-mod-verify-password)

(let ((passwords (make-collection 'linked-passwords :access-mode "700"
                                  :description "Passwords that have been linked to user accounts."
                                  :columns '((hash "700") (hash-type "700") (hash-date "700") (salt "700") (username)))))

  (defmodule verify-password ()
    "Authentication mechanism for the verify system using Passwords"
    (:fullname "Password Mechanism for Verify" 
     :author "Nicolas Hafner" 
     :version "0.0.1"
     :license "Artistic" 
     :url "http://tymoon.eu"
     
     :collections (list passwords)
     :dependencies '(verify)
     :implements '())
  
    (:components ((:file "password")))))
