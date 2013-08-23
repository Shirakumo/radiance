#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage radiance-mod-mongo
  (:use :cl :radiance :cl-mongo)
  (:export :mongodb
           :mongo-data-model))

(in-package :radiance-mod-mongo)

(implement 'database (get-module 'mongodb))

(defmethod db-connect ((db mongodb) dbname &key (host (config-tree :mongo :host))
                                             (port (config-tree :mongo :port))
                                             (user (config-tree :mongo :user))
                                             (pass (config-tree :mongo :pass)))
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
  (db.collections :mongo (dbinstance db)))

(defmethod db-create ((db mongodb) (collection string) fields &key indices (if-exists :ignore))
  "Creates a new collection on the database. Optionally a list of indexed fields can be supplied."
  (declare (ignore fields))
  (db.create-collection collection)
  (loop for index in indices
     do (destructuring-bind (keys &key drop-duplicates unique) index
          (db.ensure-index collection keys :drop-duplicates drop-duplicates :unique unique :mongo (dbinstance db)))))

(defmethod db-empty ((db mongodb) (collection string) &key)
  (db.find "$cmd" (cl-mongo::kv->ht (kv "empty" collection)) :mongo mongo))

(defmethod db-drop ((db mongodb) (collection string) &key)
  (db.run-command :drop :collection collection))

(defmethod db-select ((db mongodb) (collection string) query &key fields (skip 0) (limit 0) sort)
  "Select data from the collection. Using the iterate function is generally faster."
  (db-iterate db collection query #'identity :fields fields :skip skip :limit limit :sort sort))

(defmethod db-iterate ((db mongodb) (collection string) query function &key fields (skip 0) (limit 0) sort)
  "Iterate over a set of data. The collected return values are returned."
  (declare (ignore fields))
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (let ((result (db.find collection query :limit limit :skip skip :mongo (dbinstance db))))
    (multiple-value-bind (iterator collection docs) (cl-mongo::db.iterator result)
      (loop ; Collect all sets of records.
         for next = '(NIL (0 1)) then (db.next collection iter)
         for iter = iterator then (nth-value 0 (cl-mongo::db.iterator next))
         for idocs = docs then (append idocs (second next))
         until (zerop (length (second next)))
         finally (setf docs idocs))
      (loop ; Loop over all records.
         for doc in docs 
         collect (funcall function (document->alist doc))))))

(defmethod db-insert ((db mongodb) (collection string) (data list) &key)
  "Insert data into the collection using the rows/fields provided in data."
  (db-insert db collection (alist->document data)))

(defmethod db-insert ((db mongodb) (collection string) (data cl-mongo::document) &key)
  "Insert data into the collection using the rows/fields provided in data."
  (db.insert collection data :mongo (dbinstance db)))

(defmethod db-remove ((db mongodb) (collection string) query &key (skip 0) (limit 0) sort)
  "Remove data from the collection that matches the query. Note that if skip or limit are supplied, the delete operation will be pretty slow due to having to use a select and a remove for each match."
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (if (= 0 skip limit)
      (db.delete collection query)
      (cl-mongo:rm collection (iter (db.find collection query :limit limit :skip skip :mongo (dbinstance db))) :mongo (dbinstance db))))

(defmethod db-update ((db mongodb) (collection string) query (data list) &key (skip 0) (limit 0) sort (replace NIL) insert-inexistent)
  "Update all rows that match the query with the new data. Note that if skip or limit are supplied, the update operation will be pretty slow due to having to use a select and an update for each match."
  (db-update db collection query (alist->document data) :skip skip :limit limit :replace replace :sort sort :insert-inexistent insert-inexistent)) 

(defmethod db-update ((db mongodb) (collection string) query (data cl-mongo::document) &key (skip 0) (limit 0) sort (replace NIL) insert-inexistent)
  "Update all rows that match the query with the new data. Note that if skip or limit are supplied, the update operation will be pretty slow due to having to use a select and an update for each match."
  (db-update db collection query (cl-mongo::elements data) :limit limit :skip skip :replace replace :sort sort :insert-inexistent insert-inexistent))

(defmethod db-update ((db mongodb) (collection string) query (data hash-table) &key (skip 0) (limit 0) (replace NIL) sort insert-inexistent)
  "Update all rows that match the query with the new data. Note that if skip or limit are supplied, the update operation will be pretty slow due to having to use a select and an update for each match."
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (if (not replace) (setf data (kv "$set" data)))
  (if (and (= 0 skip limit) (not replace))
      (db.update collection query data :multi T :upsert insert-inexistent :mongo (dbinstance db))
      (let ((docs (docs (db.find collection query :limit limit :skip skip))))
        (if (= 0 (length docs))
            (if insert-inexistent (db.insert collection data))
            (loop for doc in docs do (db.update collection doc data))))))

(defmethod db-apropos ((db mongodb) (collection string) &key)
  "Always returns NIL as any field or type is allowed in MongoDB."
  NIL)

(defun document->alist (document)
  "Turns a document into an alist."
  (%document->alist document))

(defgeneric %document->alist (value))
(defmethod %document->alist (value) value)
(defmethod %document->alist ((value cl-mongo:document))
  (loop with alist = ()
     with map = (cl-mongo::elements value)
     for key being the hash-keys of map
     for val being the hash-values of map
     do (setf alist (acons key (%document->alist val) alist))
     finally (return alist)))

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

(defmacro query (&rest forms)
  (if (cdr forms)
      `(:and ,@forms)
      (first forms)))

(defun :and (&rest args) (kv "$and" args))
(defun :or  (&rest args) (kv "$or" args))
(defun :not (&rest args) `($not ,@args))
(defun := (a b) (kv a b))
(defun :< (a b) ($< a b))
(defun :> (a b) ($> a b))
(defun :>= (a b) ($>= a b))
(defun :<= (a b) ($<= a b))
(defun :in (a b) ($in a b))
(defun :matches (a b &key options) (:= a ($/ b options)))
