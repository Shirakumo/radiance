#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-mongo)
(use-package :cl-mongo)

(defvar *db* NIL)

(define-interface-method db:connect (dbname &key (host (config-tree :mongo :host))
                                            (port (config-tree :mongo :port))
                                            (user (config-tree :mongo :user))
                                            (pass (config-tree :mongo :pass)))
  (if (not host) (setf host *mongo-default-host*))
  (if (not port) (setf port *mongo-default-port*))
  (v:info :mongodb "Connecting to mongoDB on ~a:~a" host port)
  (let ((mongo (mongo :db dbname :port port :host host)))
    (setf *db* mongo)
    (when (and user pass)
      (v:info :mongodb "Authenticating with ~a/~a" user pass)
      (db.auth user pass))
    (v:info :mongodb "Switching database to ~a" dbname)
    (db.use dbname)))

(define-interface-method db:disconnect ()
  (mongo-close :default)
  (setf *db* NIL))

(define-interface-method db:connected-p ()
  (not (null *db*)))

(define-interface-method db:collections ()
  (db.collections :mongo *db*))

(define-interface-method db:create (collection fields &key indices (if-exists :ignore))
  (declare (ignore fields))
  (db.create-collection collection)
  (loop for index in indices
     do (destructuring-bind (keys &key drop-duplicates unique) index
          (db.ensure-index collection keys :drop-duplicates drop-duplicates :unique unique :mongo *db*))))

(define-interface-method db:empty (collection )
  (db.find "$cmd" (cl-mongo::kv->ht (kv "empty" collection)) :mongo mongo))

(define-interface-method db:drop (collection )
  (db.run-command :drop :collection collection))

(define-interface-method db:select (collection query &key fields (skip 0) (limit 0) sort)
  (db:iterate collection query #'identity :fields fields :skip skip :limit limit :sort sort))

(define-interface-method db:iterate (collection query function &key fields (skip 0) (limit 0) sort)
  (declare (ignore fields))
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (let ((result (db.find collection query :limit limit :skip skip :mongo *db*)))
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

(define-interface-method db:insert (collection (data list))
  (db:insert collection (alist->document data)))

(define-interface-method db:insert (collection (data cl-mongo::document))
  (db.insert collection data :mongo *db*))

(define-interface-method db:remove (collection query &key (skip 0) (limit 0) sort)
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (if (= 0 skip limit)
      (db.delete collection query)
      (cl-mongo:rm collection (iter (db.find collection query :limit limit :skip skip :mongo *db*)) :mongo *db*)))

(define-interface-method db:update (collection query (data list) &key (skip 0) (limit 0) sort (replace NIL) insert-inexistent)
  (db:update collection query (alist->document data) :skip skip :limit limit :replace replace :sort sort :insert-inexistent insert-inexistent)) 

(define-interface-method db:update (collection query (data cl-mongo::document) &key (skip 0) (limit 0) sort (replace NIL) insert-inexistent)
  (db:update collection query (cl-mongo::elements data) :limit limit :skip skip :replace replace :sort sort :insert-inexistent insert-inexistent))

(define-interface-method db:update (collection query (data hash-table) &key (skip 0) (limit 0) (replace NIL) sort insert-inexistent)
  (if sort (setf query (kv (kv "query" query) (kv "orderby" (alist->document sort)))))
  (if (not replace) (setf data (kv "$set" data)))
  (if (and (= 0 skip limit) (not replace))
      (db.update collection query data :multi T :upsert insert-inexistent :mongo *db*)
      (let ((docs (docs (db.find collection query :limit limit :skip skip))))
        (if (= 0 (length docs))
            (if insert-inexistent (db.insert collection data))
            (loop for doc in docs do (db.update collection doc data))))))

(define-interface-method db:apropos (collection)
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

(define-interface-method db:query (&rest forms)
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
