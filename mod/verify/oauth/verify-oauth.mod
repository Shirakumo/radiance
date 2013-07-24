#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.oauth
  (:nicknames :radiance-mod-verify-oauth)
  (:use :cl :radiance :lquery)
  (:export))
(in-package :radiance-mod-verify-oauth)

(let ((oauths (make-collection 'linked-oauths :access-mode "700"
                               :description "OAuth accounts that have been linked to user accounts for authentication."
                               :columns '((claimed-id) (provider) (username)))))

  (defmodule verify-oauth ()
    "Authentication mechanism for the verify system using OAuth"
    (:fullname "OAuth Mechanism for Verify" 
     :author "Nicolas Hafner" 
     :version "0.0.1"
     :license "Artistic" 
     :url "http://tymoon.eu"
     
     :collections (list oauths)
     :dependencies '(verify)
     :implements '()
     :asdf-system "radiance-mod-verify-oauth")
  
    (:components ((:file "oauth"))
     :depends-on (:radiance-mod-verify
                  :cl-oauth
                  :cl-json))))
