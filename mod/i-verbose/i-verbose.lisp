#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module i-verbose
    (:use #:cl #:radiance)
    (:implements #:logger))
(in-package #:i-verbose)

(defun l:log (level category log-string &rest format-args)
  (apply #'v:log level category log-string format-args))

(defun l:trace (category log-string &rest format-args)
  (apply #'v:log :trace category log-string format-args))

(defun l:debug (category log-string &rest format-args)
  (apply #'v:log :debug category log-string format-args))

(defun l:info (category log-string &rest format-args)
  (apply #'v:log :info category log-string format-args))

(defun l:warn (category log-string &rest format-args)
  (apply #'v:log :warn category log-string format-args))

(defun l:error (category log-string &rest format-args)
  (apply #'v:log :error category log-string format-args))

(defun l:severe (category log-string &rest format-args)
  (apply #'v:log :severe category log-string format-args))

(defun l:fatal (category log-string &rest format-args)
  (apply #'v:log :fatal category log-string format-args))
