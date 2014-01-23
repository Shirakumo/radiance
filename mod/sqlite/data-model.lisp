#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sqlite)

(defclass sqlite-data-model (data-model:class)
  ((document :initarg :document :initform (make-hash-table :test 'equal) :accessor document)
   (collection :initarg :collection :initform (error "Collection required") :accessor collection))
  (:documentation "Datamodel for sqlite."))

(defmethod print-object ((model sqlite-data-model) out)
  (print-unreadable-object (model out :type T)
    (if (collection model)
        (format out "~a" (collection model))
        (format out "STUB")))
  model)

(define-interface-method dm:field ((model sqlite-data-model) (field string) &key (value NIL v-p))
  (if v-p
      (setf (gethash field (document model)) value)
      (gethash field (document model))))

(define-interface-method dm:id ((model sqlite-data-model))
  (gethash "_id" (document model)))

(define-interface-method dm:get (collection query &key (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part queryargs) (query-to-where-part query)
    (v:trace :sqlite.model "Getting model from ~a with query ~a, skipping ~a, limiting ~a, sorting by ~a" collection query skip limit sort)
    (let* ((querystring (format NIL "SELECT * FROM `~a` ~a ~a LIMIT ~D OFFSET ~D;" collection where-part (sort-to-order-part sort) limit skip))
           (stmt (sqlite:prepare-statement *db* querystring)))
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
                                :document document)
         finally (sqlite:finalize-statement stmt)))))

(define-interface-method dm:get-one (collection query &key (skip 0) sort)
  (multiple-value-bind (where-part queryargs) (query-to-where-part query)
    (v:trace :sqlite.model "Getting one model from ~a with query ~a, skipping ~a, sorting by ~a" collection query skip sort)
    (let* ((querystring (format NIL "SELECT * FROM `~a` ~a ~a LIMIT 1 OFFSET ~D;" collection where-part (sort-to-order-part sort) skip))
           (stmt (sqlite:prepare-statement *db* querystring)))
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
                           :document document))
          NIL))))
          
(define-interface-method dm:hull (collection)
  (v:trace :sqlite.model "Creating model hull for ~a" collection)
  (make-instance 'sqlite-data-model :collection collection))

(define-interface-method dm:hull-p ((model sqlite-data-model)) 
  (eq (gethash "_id" (document model)) NIL))

(define-interface-method dm:save ((model sqlite-data-model))
  (assert (not (dm:hull-p model)) () "Model has not been inserted before.")
  (multiple-value-bind (set-part values) (model-set-part (document model))
    (db-query *db* (format NIL "UPDATE `~a` ~a WHERE `_id` = ?;" (collection model) set-part) (append values (list (dm:id model))))))

(define-interface-method dm:delete ((model sqlite-data-model))
  (assert (not (dm:hull-p model)) () "Model has not been inserted before.")
  (db-query *db* (format NIL "DELETE FROM `~a` WHERE `_id` = ?;" (collection model)) (list (dm:id model))))

(define-interface-method dm:insert ((model sqlite-data-model) &key clone)
  (loop for key being the hash-keys of (document model)
        for val being the hash-values of (document model)
        collect key into keys
        collect val into vals
        finally (db-query *db* (format NIL "INSERT INTO `~a` (~{`~a`~^, ~}) VALUES (~{?~*~^, ~});" (collection model) keys vals) vals))
  (let ((id (sqlite:last-insert-rowid *db*)))
    (if clone
        (setf model (make-instance 'sqlite-data-model 
                                   :collection (collection model)
                                   :document (alexandria:copy-hash-table (document model)))))
    (setf (gethash "_id" (document model)) id)
    model))

(defun model-where-part (document)
  (loop for where-key being the hash-keys of document
     for where-val being the hash-values of document
     for query = (concatenate 'string "WHERE `" where-key "` = ? ")
       then (concatenate 'string query "AND `" where-key "` = ? ")
     collect where-val into vals
     finally (return (values query vals))))

(defun model-set-part (document)
  (loop for set-key being the hash-keys of document
     for set-val being the hash-values of document
     for query = (concatenate 'string "SET `" set-key "` = ? ")
       then (concatenate 'string query ", `" set-key "` = ? ")
     collect set-val into vals
     finally (return (values query vals))))
