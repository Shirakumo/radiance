#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-base-profile)

(defclass base-profile (profile) ())

(implement 'profile (make-instance 'base-profile))

(defmethod profile-field ((profile base-profile) (user user) (name string) &key value default)
  (if value
      (user-field user name :value value)
      (or (user-field user name) default)))

(defmethod profile-field (profile (user T) (name string) &key value default)
  (if *radiance-session*
      (profile-field profile (session-user *radiance-session*) name :value value :default default)
      default))

(defmethod profile-avatar ((profile base-profile) (user user) (size fixnum) &key)
  (gravatar-image (user-field user "email") :size size :default :mm))

(defmethod profile-avatar (profile (user T) (size fixnum) &key)
  (if *radiance-session*
      (profile-avatar profile (session-user *radiance-session*) size)
      (gravatar-image "noop@example.com" :size size :default :mm)))

(defmethod profile-name ((profile base-profile) (user user) &key)
  (user-field user "displayname"))

(defmethod profile-name (profile (user T) &key)
  (if *radiance-session*
      (profile-name profile (session-user *radiance-session*))
      NIL))

(defmethod profile-page-settings ((profile base-profile) (user user) &key)
  )

(defmethod profile-page-user ((profile base-profile) user &key)
  )
