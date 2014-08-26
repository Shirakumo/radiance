#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module i-wookie
  (:use #:cl #:radiance)
  (:implements #:server))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (setf wookie:*enabled-plugins* '(:get :post :cookie :multipart))
  (wookie:load-plugins))
