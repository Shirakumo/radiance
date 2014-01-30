#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance.lib.crypto
  (:nicknames :radiance-lib-crypto :radiance-crypto :rad-crypto)
  (:use :cl)
  (:export
   :get-cipher
   :encrypt
   :decrypt
   :make-salt
   :pbkdf2-hash
   :simple-hash))
(in-package :radiance-lib-crypto)

(asdf:defsystem radiance-crypto
  :name "Radiance Cryptography"
  :author "Nicolas Hafner"
  :version "0.0.1"
  :license "Artistic"
  :homepage "http://tymoon.eu"
  :depends-on (:ironclad)
  :components ((:file "crypto")))
