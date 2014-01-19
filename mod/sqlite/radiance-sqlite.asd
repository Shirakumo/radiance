#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-sqlite
  (:use :cl :radiance :alexandria)
  (:export :sqlite
           :sqlite-data-model))
(in-package :radiance-mod-sqlite)

(asdf:defsystem radiance-sqlite
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :name "SQLite Binding" 
  :author "Nicolas Hafner" 
  :version "0.0.1" 
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :implement ((:database :radiance-sqlite)
              (:data-model :radiance-sqlite))
  :serial T
  :components ((:file "query")
               (:file "database")
               (:file "data-model"))
  :depends-on (:sqlite :alexandria))
