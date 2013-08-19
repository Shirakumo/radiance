#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sqlite)

(implement 'database (get-module 'sqlite))

(defmethod db-connect ((db sqlite) dbname &key (root-path (merge-pathnames "data/" (pathname (config :root)))))
  (log:info "Connecting to SQLite database ~a on ~a" dbname root-path)
  (setf (dbinstance db)
        (sqlite:connect (merge-pathnames dbname root-path))))

(defmethod db-disconnect ((db sqlite) &key)
  (log:info "Disconnecting...")
  (sqlite:disconnect (dbinstance db))
  (setf (dbinstance db) NIL))

(defmethod db-connected-p ((db sqlite) &key)
  (not (null (dbinstance db))))

(defmethod db-collections ((db sqlite) &key)
  (db-iterate db "sqlite_master" (:= "type" "table")
              #'(lambda (row) (car (assoc :test row)))))

(defun format-field-type (s arg colonp atp)
  (declare (ignore colonp atp))
  (if (listp arg)
      (case (length arg)
        (1 (format s "~a" (first arg)))
        (2 (format s "~a ~a" (first arg) (second arg)))
        (3 (format s "~a ~a(~a)" (first arg) (second arg) (third arg))))
      (format s "~a" arg)))

(defmethod db-create ((db sqlite) (collection string) fields &key indices)
  (sqlite:execute-non-query (dbinstance db) (format NIL "CREATE TABLE ~a (~{~/radiance-mod-sqlite::format-field-type/~^, ~});" collection fields))
  (if indices
      (sqlite:execute-non-query (dbinstance db) (format NIL "CREATE INDEX ~a_idx ON ~:*~a (~{~a~^, ~})" collection indices)))) 
  
(defmethod db-select ((db sqlite) (collection string) query &key fields (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (get-data db (format NIL "SELECT ~a FROM ~a ~a ~a LIMIT ~a OFFSET ~a;" (fields-to-fields-part fields) collection where-part (sort-to-order-part sort) limit skip) values)))

(defmethod db-iterate ((db sqlite) (collection string) query function &key fields (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (get-data db (format NIL "SELECT ~a FROM ~a ~a ~a LIMIT ~a OFFSET ~a;" (fields-to-fields-part fields) collection where-part (sort-to-order-part sort) limit skip) values function)))

(defmethod db-insert ((db sqlite) (collection string) data &key)
  (multiple-value-bind (set-part values) (data-to-set-part data)
    (query db (format NIL "INSERT INTO ~a ~a;" collection set-part) values)))

(defmethod db-remove ((db sqlite) (collection string) query &key (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (query db (format NIL "REMOVE FROM ~a ~a ~a LIMIT ~a OFFSET ~a;" collection where-part (sort-to-order-part sort) limit skip) values)))

(defmethod db-update ((db sqlite) (collection string) query data &key (skip 0) (limit -1) sort replace)
  (multiple-value-bind (set-part values1) (data-to-set-part data)
    (multiple-value-bind (where-part values2) (query-to-where-part query)
      (if replace
          (progn
            (query db (format NIL "REMOVE FROM ~a ~a ~a LIMIT ~a OFFSET ~a;" collection where-part (sort-to-order-part sort) limit skip) values2)
            (query db (format NIL "INSERT INTO ~a ~a;" collection set-part) values1))
          (query db (format NIL "UPDATE ~a ~a ~a ~a LIMIT ~a OFFSET ~a;" collection set-part where-part (sort-to-order-part sort) limit skip) (append values1 values2))))))

(defmethod db-apropos ((db sqlite) (collection string) &key)
  (mapcar #'second (sqlite:execute-to-list (dbinstance db) (format NIL "PRAGMA table_info(~a);" collection))))
    

(defun query-to-where-part (query)
  )

(defun data-to-set-part (data)
  (loop for (key . val) in data
     collect key into keys
     collect val into vals
     finally (return (values (format NIL "SET ~{~a = ?~^, ~}" keys) vals))))

(defun sort-to-order-part (sort)
  )

(defun fields-to-fields-part (fields)
  (etypecase fields
    (symbol (if (eq fields :all) "*" (symbol-name fields)))
    (list (format NIL "~{~a~^, ~}" fields))))

(defun :and (&rest args) )
(defun :or  (&rest args) )
(defun :not (&rest args) )
(defun := (a b) )
(defun :< (a b) )
(defun :> (a b) )
(defun :>= (a b) )
(defun :<= (a b) )
(defun :in (a b) )
(defun :!in (a b) )
(defun :matches (a b &key options) )

(defun get-data (db querystring queryargs &optional (function #'identity))
  (let* ((db (dbinstance db))
         (stmt (sqlite:prepare-statement db querystring)))
    (loop for arg in queryargs
       for i from 0
       do (sqlite:bind-parameter stmt i arg))
    (loop while (sqlite:step-statement stmt)
       collect (funcall function
                        (loop for name in (sqlite:statement-column-names stmt)
                           for i from 0
                           collect (cons name (sqlite:statement-column-value stmt i))))
       finally (sqlite:finalize-statement stmt))))

(defun query (db querystring queryargs)
  (let* ((db (dbinstance db))
         (stmt (sqlite:prepare-statement db querystring)))
    (loop for arg in queryargs
       for i from 0
       do (sqlite:bind-parameter stmt i arg))
    (sqlite:step-statement stmt)))
