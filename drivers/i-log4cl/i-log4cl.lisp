#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-log4cl
    (:use #:cl #:radiance)
    (:implements #:logger))
(in-package #:i-log4cl)

(defun l:log (level category log-string &rest format-args)
  (when (string-equal level "SEVERE") (setf level "ERROR"))
  (let ((level (find-symbol (string level) "LOG")))
    (if level
        (funcall level "[~a] ~?" category log-string format-args)
        (error "Invalid level ~a" level))))

(defun l:trace (category log-string &rest format-args)
  (log:trace "[~a] ~?" category log-string format-args))

(defun l:debug (category log-string &rest format-args)
  (log:debug "[~a] ~?" category log-string format-args))

(defun l:info (category log-string &rest format-args)
  (log:info "[~a] ~?" category log-string format-args))

(defun l:warn (category log-string &rest format-args)
  (log:warn "[~a] ~?" category log-string format-args))

(defun l:error (category log-string &rest format-args)
  (log:error "[~a] ~?" category log-string format-args))

(defun l:severe (category log-string &rest format-args)
  (log:error "[~a] ~?" category log-string format-args))

(defun l:fatal (category log-string &rest format-args)
  (log:fatal "[~a] ~?" category log-string format-args))
