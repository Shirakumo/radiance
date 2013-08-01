#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.admin
  (:nicknames :radiance-mod-admin)
  (:use :cl :radiance :lquery)
  (:export ))
(in-package :radiance-mod-admin)

(defmodule radiance-admin (admin)
  "Administrator panel for Radiance"
  (:fullname "Administrator Interface"
   :author "Nicolas Hafner"
   :version "0.0.1"
   :license "Artistic"
   :url "http://tymoon.eu"
   :dependencies '(uibox)
   :implements '(admin))
  
  (:components ((:file "admin")))

  (categories :initarg :categories :initform (make-hash-table :test 'equal) :accessor categories))
