#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem radiance-core
  :class "modularize:virtual-module"
  :defsystem-depends-on (:modularize)
  :module-name "RADIANCE-CORE"
  :version "2.2.0"
  :license "zlib"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Core component of Radiance, an extensible web application environment."
  :homepage "https://github.com/Shirakumo/radiance"
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "conditions")
               (:file "documentable")
               (:file "environment")
               (:file "interfaces")
               (:file "modules")
               (:file "uri")
               (:file "resource")
               (:file "routing")
               (:file "dispatch")
               (:file "request")
               (:file "options")
               (:file "page")
               (:file "api")
               (:file "interface-components")
               (:file "standard-interfaces")
               (:file "handle")
               (:file "defaults")
               (:file "migration")
               (:file "init")
               (:file "version-upgrades")
               (:file "documentation"))
  :depends-on (:modularize-hooks
               :modularize-interfaces
               :ubiquitous-concurrent
               :trivial-indent
               :cl-ppcre
               :trivial-mimes
               :local-time
               :lambda-fiddle
               :form-fiddle
               :bordeaux-threads
               :documentation-utils
               :babel
               :puri
               :closer-mop)
  :in-order-to ((asdf:test-op (asdf:test-op :radiance-test))))
