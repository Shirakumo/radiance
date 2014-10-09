#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(defgeneric field (model field)
  (:documentation "Attempts to extract the requested field from a variety of different data models."))
(defgeneric (setf field) (value model field))

(defmethod field ((model list) field)
  (cond
    ((keywordp (first model))
     (getf model (make-keyword field)))
    ((listp (first model))
     (cdr (assoc field model :test #'equalp)))
    (T
     (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defmethod (setf field) (value (model list) field)
  (cond
    ((keywordp (first model))
     (setf (getf model (make-keyword field)) value))
    ((listp (first model))
     (setf (cdr (assoc field model :test #'equalp)) value))
    (T
     (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defmethod field ((model hash-table) field)
  (gethash field model))

(defmethod (setf field) (value (model hash-table) field)
  (setf (gethash field model) value))

(defmethod field ((model standard-object) field)
  (let ((field (find-symbol (string-upcase field) (symbol-package (class-name (class-of model))))))
    (if field (slot-value model field))))

(defmethod (setf field) (value (model standard-object) field)
  (let ((field (find-symbol (string-upcase field) (symbol-package (class-name (class-of model))))))
    (if field (setf (slot-value model field) value))))

(defmethod field ((model asdf:component) field)
  (let ((field (find-symbol (string-upcase field) :ASDF)))
    (if field (slot-value model field))))
