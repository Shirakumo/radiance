#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-mongo)

(defclass mongo-data-model (data-model)
  ((document :initarg :document :initform (make-document :oid T) :accessor document)
   (collection :initarg :collection :initform (error "Collection required") :accessor collection))
  (:documentation "Datamodel for mongodb."))

(defmethod print-object ((model mongo-data-model) out)
  (print-unreadable-object (model out :type T)
    (if (collection model)
        (format out "~a" (collection model))
        (format out "STUB"))))

(implement 'data-model (make-instance 'mongo-data-model :collection NIL :document NIL))

(defmethod model-field ((model mongo-data-model) (field string) &key)
  "Get the value of a field in the document."
  (gethash field (cl-mongo::elements (document model))))

(defun model-field-set (model field value)
  "Set the value of a field in the document."
  (setf (gethash field (cl-mongo::elements (document model))) value))
  
(defmethod model-get ((model mongo-data-model) (collection string) query &key (skip 0) (limit 0) sort)
  "Get a model for each document in the query result."
  (db-iterate
   (implementation 'database) collection query 
   (lambda (doc) (make-instance 'mongo-data-model :collection collection :document doc))
   :sort sort :limit limit :skip skip))

(defmethod model-get-one ((model mongo-data-model) (collection string) query &key (skip 0) sort)
  "Get a model of the first result in the query."
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (let ((docs (docs (iter (db.find collection query :skip skip)))))
    (if docs (make-instance 
              'mongo-data-model :collection collection
              :document (first docs)))))

(defmethod model-hull ((model mongo-data-model) (collection string) &key)
  "Create an empty model."
  (make-instance 'mongo-data-model :collection collection))

(defmethod model-hull-p ((model mongo-data-model) &key)
  "Returns T if the model is a hull, otherwise NIL"
  (eq (doc-id (document model)) T))

(defmethod model-save ((model mongo-data-model) &key)
  "Save an existing model."
  (assert (not (eq (doc-id (document model)) T)) () "Model has not been inserted before.")
  (db.save (collection model) (document model)))

(defmethod model-delete ((model mongo-data-model) &key)
  "Delete an existing model."
  (assert (not (eq (doc-id (document model)) T)) () "Model has not been inserted before.")
  (db.delete (collection model) (kv "_id" (cl-mongo::_id (document model)))))

(defmethod model-insert ((model mongo-data-model) &key (clone NIL))
  "Insert the given model as a new entry and return it. If clone is T, a new copy of the document is created and the original is left untouched."
  (if clone 
      (setf model (make-instance 'mongo-data-model :collection (collection model) :document (clone-document (document model))))
      (setf (slot-value (document model) 'cl-mongo::_id) (cl-mongo::make-bson-oid)))
  (db.insert (collection model) (document model))
  model)

(defmacro with-fields ((&rest fields) model &rest body)
  "Lets you access fields directly by name. This is the same stuff as with-accessors or with-slots."
  (let ((vargens (gensym "MODEL")))
    `(let ((,vargens ,model))
       (symbol-macrolet ,(loop for field in fields collect `(,field (model-field ,vargens ,(string-downcase (symbol-name field)))))
         ,@body))))

(defsetf model-field model-field-set)

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
