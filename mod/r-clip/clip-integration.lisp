#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module r-clip
  (:use #:cl #:radiance)
  (:export #:process))
(in-package #:r-clip)

(define-page-option lquery (page uri &optional template)
  (when template
    (setf *page-body*
          `((setf (content-type *response*) "application/xhtml+xml")
            (let* ((lquery:*lquery-master-document*
                    (lquery:load-page ,template)))
              ,@*page-body*
              (lquery:$ (serialize) (node))))))
  NIL)

(defmethod clip:clip ((object standard-object) field)
  (field object field))

(defun process (target &rest fields)
  (let ((*package* (find-package "RADIANCE")))
    (apply #'clip:process target fields)))

(defmacro lquery-wrapper ((template) &body body)
  `(let ((lquery:*lquery-master-document* (lquery:load-page (template ,template))))
     ,@body
     (setf (content-type *response*) "application/xhtml+xml")
     (lquery:$ (serialize) (node))))
