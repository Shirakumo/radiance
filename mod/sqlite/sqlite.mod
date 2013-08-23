#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-sqlite
  (:use :cl :radiance)
  (:export :sqlite
           :sqlite-data-model))

(in-package :radiance-mod-sqlite)

(defmodule sqlite (database)
  "Database implementation for SQLite"
  (:fullname "SQLite Binding" 
   :author "Nicolas Hafner" 
   :version "0.0.1" 
   :license "Artistic" 
   :url "http://tymoon.eu"

   :implements '(database data-model))
   
  (:serial T
   :components ((:file "query")
                (:file "database")
                (:file "data-model"))
   :depends-on (:sqlite))

  (dbinstance :initform NIL :initarg dbinstance :accessor dbinstance))
