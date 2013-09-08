#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.db-introspect
  (:nicknames :radiance-mod-db-introspect)
  (:use :cl :radiance :lquery))
(in-package :radiance-mod-db-introspect)

(defmodule db-introspect ()
  "Provides admin panels to manipulate the database."
  (:fullname "Database Introspect"
   :author "Nicolas Hafner"
   :version "0.0.1"
   :license "Artistic"
   :url "http://tymoon.eu"
   :dependencies '(admin uibox)
   :implements '())
  
  (:components ((:file "admin"))
   :depends-on (:string-case)))
