#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem radiance-core
  :class "modularize:virtual-module"
  :defsystem-depends-on (:modularize)
  :module-name "RADIANCE-CORE"
  :version "1.5.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "Core component of Radiance, an extensible web application environment."
  :serial T
  :components ((:file "module")
               (:file "toolkit")
               (:file "conditions")
               (:file "documentable")
               (:file "environment")
               (:file "interfaces")
               (:file "modules")
               (:file "migration")
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
               :closer-mop)
  :in-order-to ((asdf:test-op (asdf:test-op :radiance-test))))
