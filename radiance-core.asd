(asdf:defsystem radiance-core
  :class "modularize:virtual-module"
  :defsystem-depends-on (:modularize :deploy)
  :module-name "RADIANCE-CORE"
  :version "2.2.0"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "Core component of Radiance, an extensible web application environment."
  :homepage "https://github.com/Shirakumo/radiance"
  :build-operation "deploy-op"
  :build-pathname #+linux "radiance-linux.run"
                  #+darwin "radiance-macos"
                  #+win32 "radiance-windows"
                  #+(and bsd (not darwin)) "radiance-bsd.run"
  :entry-point "org.shirakumo.radiance.core::startup-binary"
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
               (:file "deploy")
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
