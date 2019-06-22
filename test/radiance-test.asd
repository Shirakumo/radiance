#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem radiance-test
  :class "radiance:virtual-module"
  :defsystem-depends-on (:radiance)
  :version "0.0.2"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Test suite system for Radiance"
  :serial T
  :components ((:file "package")
               (:file "radiance")
               (:file "database")
               (:file "ban")
               (:file "cache")
               (:file "server"))
  :depends-on ((:interface :ban)
               (:interface :rate)
               (:interface :admin)
               (:interface :cache)
               (:interface :auth)
               (:interface :session)
               (:interface :user)
               (:interface :profile)
               (:interface :server)
               (:interface :logger)
               (:interface :database)
               :parachute
               :alexandria
               :dexador
               :cl-ppcre
               :verbose)
  :perform (asdf:test-op (op c) (uiop:symbol-call :radiance-test :run-test)))
