#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.verify.oauth
  (:nicknames :radiance-mod-verify-oauth)
  (:use :cl :radiance :lquery :radiance-mod-verify :alexandria)
  (:export))
(in-package :radiance-mod-verify-oauth)

(asdf:defsystem verify-oauth
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "OAuth Mechanism for Verify" 
  :author "Nicolas Hafner" 
  :version "0.0.1"
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :components ((:file "oauth"))
  :depends-on (:verify
               :cl-oauth
               :cl-json
               :alexandria))
