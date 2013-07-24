#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.mod.kickstart
  (:nicknames :radiance-mod-kickstart)
  (:use :cl :radiance)
  (:export ))
(in-package :radiance-mod-kickstart)

(let ((collection (make-collection 'kickstart-starters :access-mode "775" :description "Dispatch table for kickstar-dispatch."
                                   :columns '((subdomains) (domain) (port) (pathregex)))))
  (defmodule kickstart (dispatcher)
    "Kickstart dispatcher module to allow dynamic dispatching."
    (:fullname "Kickstart Dispatcher"
     :author "Nicolas Hafner"
     :version "0.0.1"
     :license "Artistic"
     :url "http://tymoon.eu"
     
     :collections (list collection)
     :dependencies '(data-model)
     :implements '(dispatcher))

    (:components ((:file "dispatcher")))))
