(asdf:defsystem radiance
  :version "2.1.2"
  :license "zlib"
  :author "Yukari Hafner <shinmera@tymoon.eu>"
  :maintainer "Yukari Hafner <shinmera@tymoon.eu>"
  :description "A web application environment."
  :homepage "https://shirakumo.org/project/radiance"
  :serial T
  :depends-on (:modularize
               :deploy
               :radiance-core)
  :in-order-to ((asdf:test-op (asdf:test-op :radiance-core))
                (asdf:build-op (asdf:build-op :radiance-core))))
