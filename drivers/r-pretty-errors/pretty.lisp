#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
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

(defun handle-condition (condition)
  (l:warn :radiance "Handling stray condition: ~a" condition)
  (if *debugger*
      (invoke-debugger condition)
      (invoke-restart
       'radiance::set-data
       (with-output-to-string (stream)
         (plump:serialize
          (r-clip:process
           (plump:parse (template "error.ctml"))
           :condition condition
           :stack (dissect::stack)
           :restarts (dissect::restarts)
           :objects (remove-if #'null (list condition *request* *response* *session*))) stream)))))
