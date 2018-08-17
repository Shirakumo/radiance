#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem radiance
  :version "2.0.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A web application environment."
  :homepage "https://shirakumo.github.io/radiance/"
  :bug-tracker "https://github.com/Shirakumo/radiance/issues"
  :source-control (:git "https://github.com/Shirakumo/radiance.git")
  :serial T
  :depends-on (:modularize
               :radiance-core)
  :in-order-to ((asdf:test-op (asdf:test-op :radiance-core))))
