#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-mongo
  (:use :cl :radiance)
  (:export :mongodb
           :mongo-data-model))

(in-package :radiance-mod-mongo)

(defmodule mongodb (database)
  "Database implementation for MongoDB"
  (:fullname "MongoDB Binding" 
   :author "Nicolas Hafner" 
   :version "0.0.1" 
   :license "Artistic" 
   :url "http://tymoon.eu"

   :implements '(database data-model)
   :asdf-system "radiance-mod-mongo")

  (dbinstance :initform NIL :initarg dbinstance :accessor dbinstance))
