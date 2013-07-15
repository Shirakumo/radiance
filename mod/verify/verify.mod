#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify
  (:nicknames :radiance-mod-verify)
  (:use :cl :radiance)
  (:export :verify-auth :verify-user :verify-session :verify-impl))
(in-package :radiance-mod-verify)

(defmodule verify (auth)
  "Verification Module to provide user, session and authentication parts."
  (:fullname "Verify Authentication System" 
   :author "Nicolas Hafner" 
   :version "0.0.1"
   :license "Artistic" 
   :url "http://tymoon.eu"
   
   :dependencies '(data-model)
   :implements '(user session auth)
   :asdf-system "radiance-mod-verify"))
