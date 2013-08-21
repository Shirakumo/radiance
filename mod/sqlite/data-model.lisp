#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sqlite)

(defclass sqlite-data-model (data-model)
  ((document :initarg :document :initform (make-hash-table :test 'equal) :accessor document)
   (delta :initarg :delta :initform (make-hash-table :test 'equal) :accessor delta)
   (collection :initarg :collection :initform (error "Collection required") :accessor collection)
   (hull-p :initarg :hull-p :initform T :accessor hull-p))
  (:documentation "Datamodel for sqlite."))

(defmethod print-object ((model sqlite-data-model) out)
  (print-unreadable-object (model out :type T)
    (if (collection model)
        (format out "~a" (collection model))
        (format out "STUB"))))

(implement 'data-model (make-instance 'sqlite-data-model :collection NIL :document NIL))

(defmethod model-field ((model sqlite-data-model) (field string) &key (value NIL v-p))
  (if v-p
      (setf (gethash field (delta model)) value)
      (or (gethash field (delta model))
          (gethash field (document model)))))

(defun model-field-set (model field value)
  "Set the value of a field in the document."
  (setf (gethash field (delta model)) value))

(defsetf model-field model-field-set)

(defmethod model-get ((model sqlite-data-model) (collection string) query &key (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part queryargs) (query-to-where-part query)
    (let* ((db (dbinstance (get-module :sqlite)))
           (querystring (format NIL "SELECT * FROM ~a ~a ~a LIMIT ~D OFFSET ~D;" collection where-part (sort-to-order-part sort) limit skip))
           (stmt (sqlite:prepare-statement db querystring)))
      (loop for arg in queryargs
         for i from 1
         do (sqlite:bind-parameter stmt i arg))
      (loop while (sqlite:step-statement stmt)
         for document = (loop with model = (make-hash-table :test 'equal) 
                           for name in (sqlite:statement-column-names stmt)
                           for i from 0
                           do (setf (gethash name model) (sqlite:statement-column-value stmt i))
                           finally (return model))
         collect (make-instance 'sqlite-data-model
                                :collection collection
                                :document document
                                :hull-p NIL)
         finally (sqlite:finalize-statement stmt)))))

(defmethod model-get-one ((model sqlite-data-model) (collection string) query &key (skip 0) sort)
  (multiple-value-bind (where-part queryargs) (query-to-where-part query)
    (let* ((db (dbinstance (get-module :sqlite)))
           (querystring (format NIL "SELECT * FROM ~a ~a ~a LIMIT 1 OFFSET ~D;" collection where-part (sort-to-order-part sort) skip))
           (stmt (sqlite:prepare-statement db querystring)))
      (loop for arg in queryargs
         for i from 1
         do (sqlite:bind-parameter stmt i arg))
      (if (sqlite:step-statement stmt)
          (let ((document (loop with model = (make-hash-table :test 'equal) 
                                                   for name in (sqlite:statement-column-names stmt)
                                                   for i from 0
                                                   do (setf (gethash name model) (sqlite:statement-column-value stmt i))
                                                   finally (return model))))
            (sqlite:finalize-statement stmt)
            (make-instance 'sqlite-data-model
                           :collection collection
                           :document document
                           :hull-p NIL))
          NIL))))
          
(defmethod model-hull ((model sqlite-data-model) (collection string) &key)
  (make-instance 'sqlite-data-model :collection collection :hull-p T))

(defmethod model-hull-p ((model sqlite-data-model) &key) 
  (hull-p model))

(defmethod model-save ((model sqlite-data-model) &key)
  (multiple-value-bind (set-part values2) (model-set-part (delta model))
    (multiple-value-bind (where-part values1) (model-where-part (document model))
      (db-query (get-module :sqlite) (format NIL "UPDATE ~a ~a ~a LIMIT 1;" (collection model) set-part where-part) (append values1 values2))
      (setf (document model) (delta model)
            (delta model) (make-hash-table :test 'equal)
            (hull-p model) NIL))))

(defmethod model-delete ((model sqlite-data-model) &key)
  (multiple-value-bind (where-part values) (model-where-part (delta model))
    (db-query (get-module :sqlite) (format NIL "DELETE FROM ~a ~a LIMIT 1;" (collection model) where-part) values)))

(defmethod model-insert ((model sqlite-data-model) &key clone)
  )

(defun model-where-part (document)
  (loop for where-key being the hash-keys of document
     for where-val being the hash-values of document
     for query = (concatenate 'string "WHERE " where-key " = ? ")
       then (concatenate 'string query "AND " where-key " = ? ")
     collect where-val into vals
     finally (return (values query vals))))

(defun model-set-part (document)
  (loop for set-key being the hash-keys of document
     for set-val being the hash-values of document
     for query = (concatenate 'string "SET " set-key " = ? ")
       then (concatenate 'string ", " set-key " = ? ")
     collect set-val into vals
     finally (return (values query vals))))
