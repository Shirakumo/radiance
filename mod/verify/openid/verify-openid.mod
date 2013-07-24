#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.openid
  (:nicknames :radiance-mod-verify-openid)
  (:use :cl :radiance :lquery)
  (:export))
(in-package :radiance-mod-verify-openid)

(let ((openids (make-collection 'linked-openids :access-mode "700"
                                :description "OpenID accounts that have been linked to user accounts for authentication."
                                :columns '((claimed-id) (username)))))
  
  (defmodule verify-openid ()
    "Authentication mechanism for the verify system using OpenID"
    (:fullname "OpenID Mechanism for Verify" 
     :author "Nicolas Hafner" 
     :version "0.0.1"
     :license "Artistic" 
     :url "http://tymoon.eu"
     
     :collections (list openids)
     :dependencies '(verify)
     :implements '()
     :asdf-system "radiance-mod-verify-openid")

    (:components ((:file "openid"))
     :depends-on (:radiance-mod-verify
                  :cl-openid
                  :puri))))
