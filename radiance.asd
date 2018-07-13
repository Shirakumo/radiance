#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(asdf:defsystem radiance
  :version "1.3.0"
  :license "Artistic"
  :author "Nicolas Hafner <shinmera@tymoon.eu>"
  :maintainer "Nicolas Hafner <shinmera@tymoon.eu>"
  :description "A web application environment."
  :serial T
  :depends-on (:modularize
               :radiance-core)
  :in-order-to ((asdf:test-op (asdf:test-op :radiance-core))))
