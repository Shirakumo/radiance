#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:pretty-errors
  (:use #:cl #:radiance))
(in-package #:pretty-errors)

(defun format-time (ut)
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~}"
          (subseq (nreverse (multiple-value-list (decode-universal-time ut))) 3)))

(defun slots (object)
  (when (ignore-errors (class-of object)) ;; The CLHS doesn't specify what happens if there is no class.
    (loop for slot in (c2mop:class-slots (class-of object))
          collect (list (c2mop:slot-definition-name slot)
                        (slot-value object (c2mop:slot-definition-name slot))))))

(define-page show-error #@"/error" (:lquery (template "error.ctml"))
  (r-clip:process
   T
   :stack (dissect::stack)
   :restarts (dissect::restarts)
   :objects (remove-if #'null (list *request* *response* *session*))))
