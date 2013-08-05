(defpackage org.tymoonnext.radiance.mod.votey
  (:use :cl :radiance :lquery)
  (:nicknames :radiance-mod-votey))

(in-package :radiance-mod-votey)

(defmodule votey ()
  "Vote stuff"
  (:fullname "Votey" 
   :author "Nicolas Hafner" 
   :version "0.0.1" 
   :license "Artistic" 
   :url "http://tymoon.eu"

   :dependencies '(data-model database uibox))
   
  (:components ((:file "votey"))))
