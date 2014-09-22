#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-json
    (:use #:cl #:radiance))
(in-package #:i-json)

(define-api-format json (object)
  (setf (content-type *response*) "application/json")
  (with-output-to-string (stream)
    (cl-json:encode-json object stream)))

(setf *default-api-format* "json")
