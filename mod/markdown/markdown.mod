#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defpackage :org.tymoonnext.radiance.mod.markdown
  (:use :cl :radiance)
  (:nicknames :radiance-mod-markdown))

(in-package :radiance-mod-markdown)

(defmodule markdown (parser)
  "Markdown parser module."
  (:fullname "Markdown" 
   :author "Nicolas Hafner" 
   :version "0.0.1"
   :license "Artistic" 
   :url "http://tymoon.eu"

   :implements '(parser)
   :dependencies ())
   
  (:components ((:file "parser"))
   :depends-on (:cl-markdown)))
