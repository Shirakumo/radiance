#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module r-clip
  (:use #:cl #:radiance)
  (:export #:process #:lquery-wrapper))
(in-package #:r-clip)

(defmethod clip:clip ((object standard-object) field)
  (field object field))

(defun process (target &rest fields)
  (let ((*package* (find-package "RADIANCE-USER")))
    (apply #'clip:process
           (if (eql target T)
               lquery:*lquery-master-document*
               target)
           fields)))

(defmacro lquery-wrapper ((template &optional (content-type "application/xhtml+xml")) &body body)
  `(let ((lquery:*lquery-master-document* (lquery:load-page (template ,template))))
     ,@body
     (setf (content-type *response*) ,content-type)
     (lquery:$ (serialize) (node))))

(defun transform-body (body template)
  (if template
      `((let* ((lquery:*lquery-master-document*
                 (lquery:load-page ,template)))
          ,@body
          (lquery:$ (serialize) (node))))
      body))

(define-page-option lquery (page uri body template)
  (if template
      `((setf (content-type *response*) "application/xhtml+xml")
        ,@(transform-body body template))
      body))

(define-implement-hook admin
  (admin:define-panel-option lquery (name category body template)
    (transform-body body template)))

(define-implement-hook profile
  (profile:define-panel-option lquery (name body template)
    (transform-body body template)))


