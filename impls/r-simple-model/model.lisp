#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module simple-model
  (:use #:cl #:radiance)
  (:implements #:data-model))
(in-package #:simple-model)

(defclass data-model (dm:data-model)
  ((collection :initform (error "COLLECTION required.") :initarg :collection :accessor collection)
   (fields :initform (make-hash-table :test 'equalp) :initarg :fields :accessor fields)
   (inserted :initform NIL :initarg :inserted :accessor inserted)))

(defmethod print-object ((model data-model) stream)
  (print-unreadable-object (model stream :type NIL)
    (format stream "DATA-MODEL ~a:~a~@[ HULL~]" (collection model) (dm:id model) (dm:hull-p model))))

(defun dm:id (data-model)
  (gethash "_id" (fields data-model)))

(defun dm:collection (data-model)
  (collection data-model))

(defun dm:field (data-model field)
  (gethash (string-downcase field) (fields data-model)))

(defun (setf dm:field) (value data-model field)
  (setf (gethash (string-downcase field) (fields data-model)) value))

(defun dm:get (collection query &key (skip 0) amount sort)
  (db:iterate collection query #'(lambda (ta) (make-instance 'data-model :collection collection :fields ta :inserted T))
              :skip skip :amount amount :sort sort :accumulate T))

(defun dm:get-one (collection query &key (skip 0) sort)
  (db:iterate collection query #'(lambda (ta) (return-from dm:get-one
                                                (make-instance 'data-model :collection collection :fields ta :inserted T)))
              :skip skip :amount 1 :sort sort))

(defun dm:hull (collection) ;; speed up test with extra interface func.
  (unless (db:collection-exists-p collection)
    (error 'database-invalid-collection :collection collection :message "Cannot create hull."))
  (make-instance 'data-model :collection collection))

(defun dm:hull-p (data-model)
  (not (inserted data-model)))

(defun dm:save (data-model)
  (unless (inserted data-model)
    (error 'data-model-not-inserted-yet :model data-model))
  (db:update (collection data-model) (db:query (:= '_id (dm:id data-model))) (fields data-model))
  data-model)

(defun dm:delete (data-model)
  (unless (inserted data-model)
    (error 'data-model-not-inserted-yet :model data-model))
  (db:remove (collection data-model) (db:query (:= '_id (dm:id data-model))))
  (setf (inserted data-model) NIL)
  data-model)

(defun copy-hash-table (table)
  (let ((n (make-hash-table :test 'equalp)))
    (maphash #'(lambda (k v) (setf (gethash k n) v)) table)
    n))

(defun copy-model (model)
  (make-instance 'data-model
                 :inserted (inserted model)
                 :fields (copy-hash-table (fields model))
                 :collection (collection model)))

(defun dm:insert (data-model &key clone)
  (let ((data-model (if clone (copy-model data-model) data-model)))
    (remhash "_id" (fields data-model))
    (setf (dm:field data-model "_id")
          (db:insert (collection data-model)
                     (fields data-model)))
    data-model))

(defun dm::read (readable)
  (destructuring-bind (type collection inserted data) readable
    (assert (eql type :DM))
    (make-instance 'data-model :inserted inserted :collection collection
                                  :fields (loop with table = (make-hash-table :test 'equalp)
                                                for (k . v) in data
                                                do (setf (gethash k table) v)
                                                finally (return table)))))

(defun dm::print (data-model)
  (list :dm (collection data-model) (inserted data-model)
        (loop for k being the hash-keys of (fields data-model)
              for v being the hash-values of (fields data-model)
              collect (cons k v))))

(defmethod field ((model data-model) field)
  (gethash (string-downcase field) (fields model)))

(defmethod (setf field) (value (model data-model) field)
  (setf (gethash (string-downcase field) (fields model)) value))
