#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-sqlite
  (:use #:cl #:radiance)
  (:implements #:database)
  (:export
   #:*sqlite-pcre-paths*))
