#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.oauth
  (:nicknames :radiance-mod-verify-oauth)
  (:use :cl :radiance)
  (:export))
(in-package :radiance-mod-verify-oauth)

(defmodule verify-oauth ()
  "Authentication mechanism for the verify system using OAuth"
  (:fullname "OAuth Mechanism for Verify." 
   :author "Nicolas Hafner" 
   :version "0.0.1"
   :license "Artistic" 
   :url "http://tymoon.eu"
   
   :dependencies '(verify)
   :implements '()
   :asdf-system "radiance-mod-verify-oauth"))
