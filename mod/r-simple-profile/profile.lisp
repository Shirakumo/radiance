#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module simple-profile
  (:use #:cl #:radiance)
  (:implements #:profile))
(in-package #:simple-profile)

(defun profile:avatar (user size)
  (let ((email (or (user:field (if (stringp user) (user:get user) user) "email") "")))
    (format NIL "//www.gravatar.com/avatar/~a?s=~d&d=blank"
            (cryptos:md5 (string-downcase email)) size)))

(defun profile:name (user)
  (user:field user "displayname"))

(defun profile:page (user &optional (page-type :profile))
  (declare (ignore user page-type))
  "")

(defmacro profile:define-profile-panel (name options &body body)
  (declare (ignore name options body)))

(defmacro profile:define-settings-panel (name options &body body)
  (declare (ignore name options body)))
