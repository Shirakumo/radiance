(defpackage org.tymoonnext.radiance.mod.votey
  (:use :cl :radiance :lquery)
  (:nicknames :radiance-mod-votey))

(in-package :radiance-mod-votey)

(asdf:defsystem votey
  :class :radiance-module
  :defsystem-depends-on (:radiance)
  :fullname "Votey" 
  :author "Nicolas Hafner" 
  :version "0.0.1" 
  :license "Artistic" 
  :homepage "http://tymoon.eu"
  :dependends-on (:radiance-data-model
                  :radiance-database
                  :uibox) 
  :components ((:file "votey")))
