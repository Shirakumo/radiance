#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.ticker
  (:nicknames :radiance-mod-ticker)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-ticker)

(asdf:defsystem ticker
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "Ticker"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :depends-on (:radiance-core)
  :components ((:file "api")
               (:file "")))
