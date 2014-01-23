#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-mongo)

(defclass mongo-data-model (data-model:class)
  ((document :initarg :document :initform (make-document :oid T) :accessor document)
   (collection :initarg :collection :initform (error "Collection required") :accessor collection))
  (:documentation "Datamodel for mongodb."))

(defmethod print-object ((model mongo-data-model) out)
  (print-unreadable-object (model out :type T)
    (if (collection model)
        (format out "~a" (collection model))
        (format out "STUB")))
  model)

(define-interface-method dm:field ((model mongo-data-model) (field string) &key (value NIL v-p))
  (if v-p
      (setf (gethash field (cl-mongo::elements (document model))) value)
      (gethash field (cl-mongo::elements (document model)))))

(define-interface-method dm:id ((model mongo-data-model))
  (slot-value (document model) 'cl-mongo::_id))
  
(define-interface-method dm:get (collection query &key (skip 0) (limit 0) sort)
  (db-iterate
   (implementation 'database) collection query 
   (lambda (doc) (make-instance 'mongo-data-model :collection collection :document doc))
   :sort sort :limit limit :skip skip))

(define-interface-method dm:get-one (collection query &key (skip 0) sort)
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (let ((docs (docs (iter (db.find collection query :skip skip)))))
    (if docs (make-instance 
              'mongo-data-model :collection collection
              :document (first docs)))))

(define-interface-method dm:hull ((collection string))
  (make-instance 'mongo-data-model :collection collection))

(define-interface-method dm:hull-p ((model mongo-data-model))
  (eq (doc-id (document model)) T))

(define-interface-method dm:save ((model mongo-data-model))
  (assert (not (eq (doc-id (document model)) T)) () "Model has not been inserted before.")
  (db.save (collection model) (document model)))

(define-interface-method dm:delete ((model mongo-data-model))
  (assert (not (eq (doc-id (document model)) T)) () "Model has not been inserted before.")
  (db.delete (collection model) (kv "_id" (cl-mongo::_id (document model)))))

(define-interface-method dm:insert ((model mongo-data-model) &key (clone NIL))
  (if clone 
      (setf model (make-instance 'mongo-data-model :collection (collection model) :document (clone-document (document model))))
      (setf (slot-value (document model) 'cl-mongo::_id) (cl-mongo::make-bson-oid)))
  (db.insert (collection model) (document model))
  model)

(defgeneric clone-document (var))

(defmethod clone-document (var) var)

(defmethod clone-document ((var cons))
  (if (listp (cdr var))
      (loop for item in var collect (clone-document item))
      (cons (car var) (clone-document (cdr var)))))

(defmethod clone-document ((document document))
  (let ((doc (make-document)))
    (setf (cl-mongo::elements doc)
          (clone-document (cl-mongo::elements document))) 
    doc))

(defmethod clone-document ((table hash-table))
  (loop with trg = (make-hash-table :test 'equal)
       for key being the hash-keys of table
       for val being the hash-values of table
       do (setf (gethash key trg) (clone-document val))
       finally (return trg)))
