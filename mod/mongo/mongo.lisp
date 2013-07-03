#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-mongo
  (:use :cl :radiance :cl-mongo)
  (:export :mongodb))

(in-package :radiance-mod-mongo)

(defmodule mongodb (database)
  "Database implementation for MongoDB"
  (:name "MongoDB Binding" :author "Nicolas Hafner" :version "0.0.1" :license "Artistic" :url "http://tymoon.eu")
  (dbinstance :initform NIL :initarg dbinstance :accessor dbinstance))

(implement 'database (get-module 'mongodb))

(defmethod db-connect ((db mongodb) dbname &key (host (config-tree :database :host))
                                             (port (config-tree :database :port))
                                             (user (config-tree :database :user))
                                             (pass (config-tree :database :pass)))
  "Connects to the mongodb."
  (if (not host) (setf host *mongo-default-host*))
  (if (not port) (setf port *mongo-default-port*))
  (log:info "Connecting to mongoDB on ~a:~a" host port)
  (let ((mongo (mongo :db dbname :port port :host host)))
    (setf (dbinstance db) mongo)
    (when (and user pass)
      (log:info "Authenticating with ~a/~a" user pass)
      (db.auth user pass))
    (log:info "Switching database to ~a" dbname)
    (db.use dbname)))

(defmethod db-disconnect ((db mongodb) &key)
  "Disconnects from mongodb."
  (mongo-close :default)
  (setf (dbinstance db) NIL))

(defmethod db-connected-p ((db mongodb) &key)
  "Returns T if a connection exists, NIL otherwise."
  (if (dbinstance db) T NIL))

(defmethod db-collections ((db mongodb) &key)
  "Returns a list of all collection names available in the database."
  (db.collections))

(defmethod db-create ((db mongodb) (collection collection) &key indices)
  "Creates a new collection on the database. Optionally a list of indexed fields can be supplied."
  (let ((colname (slot-value collection 'name)))
    (db.create-collection colname)
    (loop for index in indices
         do (destructuring-bind (keys &key drop-duplicates unique) index
              (db.ensure-index colname keys :drop-duplicates drop-duplicates :unique unique)))))

(defmethod db-select ((db mongodb) (collection collection) query &key (skip 0) (limit 0) sort)
  "Select data from the collection. Using the iterate function is generally faster."
  (db-iterate db collection query #'document->alist :skip skip :limit limit :sort sort))

;@todo
(defmethod db-iterate ((db mongodb) (collection collection) query function &key (skip 0) (limit 0) sort)
  "Iterate over a set of data. The collected return values are returned."
  )

(defmethod db-insert ((db mongodb) (collection collection) data &key)
  "Insert data into the collection using the rows/fields provided in data."
  (db.insert (slot-value collection 'name) (alist->document data)))

;@todo
(defmethod db-remove ((db mongodb) (collection collection) query &key (skip 0) (limit 0))
  "Remove data from the collection that matches the query. Performs a db-select internally."
  (db.delete (slot-value collection 'name) (db-select db collection query :skip skip :limit limit)))

;@todo
(defmethod db-update ((db mongodb) (collection collection) query data &key (skip 0) (limit 0))
  "Update all rows that match the query with the new data."
  (db.update (slot-value collection 'name) query (alist->document data) :multi T))

(defmethod db-apropos ((db mongodb) (collection collection) &key)
  "Always returns NIL as any field or type is allowed in MongoDB."
  NIL)

(defun document->alist (document)
  "Turns a document into an alist."
  document)

(defun alist->document (alist)
  "Turns an alist into a document."
  (loop with doc = (make-document)
     for (key . val) in alist
     do (add-element key (%alist->document val) doc)
     finally (return doc)))

(defgeneric %alist->document (value))
(defmethod %alist->document (value) value)
(defmethod %alist->document ((value cons))
  (if (listp (cdr value))
      (loop for val in value collect (%alist->document val))
      (add-element (car value) (%alist->document (cdr value)) (make-document))))

(defmacro query (&rest funcs)
  "Construct a query parameter. See the spec for more information on how to use it."
  (case (length funcs)
    (0 'all)
    (1 (%query-part (car (first funcs)) (cdr (first funcs))))
    (otherwise (kv "$and" (loop for func in funcs collect (%query-part (car func) (cdr func)))))))

(defgeneric %query-part (func args))

(defmethod %query-part ((func symbol) args)
  (error "Function ~a unknown." func))

(defmethod %query-part ((func (eql 'or)) args)
  (kv "$or" (loop for func in args collect (%query-part (car func) (cdr func)))))

(defmethod %query-part ((func (eql 'and)) args)
  (kv "$and" (loop for func in args collect (%query-part (car func) (cdr func)))))

(defmethod %query-part ((func (eql 'not)) args)
  ($not (%query-part (car args) (cdr args))))

(defmethod %query-part ((func (eql '=)) args)
  (kv (first args) (second args)))

(defmethod %query-part ((func (eql '!=)) args)
  ($!= (first args) (second args)))

(defmethod %query-part ((func (eql '>)) args)
  ($> (first args) (second args)))

(defmethod %query-part ((func (eql '<)) args)
  ($< (first args) (second args)))

(defmethod %query-part ((func (eql '>=)) args)
  ($>= (first args) (second args)))

(defmethod %query-part ((func (eql '<=)) args)
  ($<= (first args) (second args)))

(defmethod %query-part ((func (eql 'in)) args)
  ($in (first args) (second args)))

(defmethod %query-part ((func (eql '!in)) args)
  ($!in (first args) (second args)))

(defmethod %query-part ((func (eql 'matches)) args)
  ($/ (first args) (second args)))
