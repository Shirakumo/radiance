#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)


(define-hook (:server :init) (:documentation "Database initializer for trivial-profile.")
  (db:create "trivial-profile-fields" '(("field" :varchar 32) ("value" :text) ("type" :varchar 16) ("public" :varchar 3)) :indices '("field"))
  (db:create "trivial-profile" '(("user" :varchar 32) ("field" :varchar 32) ("value" :text)) :indices '("user")))

(define-interface-method profile:field ((user user:class) (name string) &key value default)
  (if value
      (user:field user name :value value)
      (or (user:field user name) default)))

(define-interface-method profile:field ((user T) (name string) &key value default)
  (if *radiance-session*
      (profile:field (session:user *radiance-session*) name :value value :default default)
      default))

(define-interface-method profile:avatar ((user user:class) (size fixnum) &key)
  (gravatar-image (user:field user "email") :size size :default :mm))

(define-interface-method profile:avatar ((user T) (size fixnum) &key)
  (if *radiance-session*
      (profile:avatar (session:user *radiance-session*) size)
      (gravatar-image "noop@example.com" :size size :default :mm)))

(define-interface-method profile:name ((user user:class) &key)
  (user:field user "displayname"))

(define-interface-method profile:name ((user T) &key)
  (if *radiance-session*
      (profile:name (session:user *radiance-session*))
      NIL))
