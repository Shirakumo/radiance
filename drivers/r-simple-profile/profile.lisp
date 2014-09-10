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

(defun normalize (user)
  (etypecase user
    (user:user user)
    (null (user:get :anonymous))
    ((or string symbol) (user:get user :if-does-not-exist :error))))

(defun profile:avatar (user size)
  (let ((email (or (user:field (normalize user) "email") "")))
    (format NIL "//www.gravatar.com/avatar/~a?s=~d&d=blank"
            (cryptos:md5 (string-downcase email)) size)))

(defun profile:name (user)
  (user:field (normalize user) "displayname"))

(defun profile:page (user &optional (page-type :profile))
  (declare (ignore user page-type))
  "")
