#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sqlite)

(defvar *db* NIL)

(define-condition pcre-not-found (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "Could not find sqlite3 pcre extension library."))))

(cffi:defcfun sqlite3-enable-load-extension :int
  (*db* sqlite-ffi:p-sqlite3)
  (onoff :int))

(defun load-extension (*db* extensionpath)
  (v:debug :sqlite "Loading sqlite extension ~a" extensionpath)
  (sqlite3-enable-load-extension (sqlite::handle *db*) 1)
  (sqlite:execute-non-query *db* (format NIL "SELECT load_extension('~a');" extensionpath))
  (sqlite3-enable-load-extension (sqlite::handle *db*) 0))

(define-interface-method db:connect (dbname &key (root-path (merge-pathnames "data/" (pathname (config :root)))))
  (v:info :sqlite "Connecting to SQLite database ~a on ~a" dbname root-path)
  (setf *db* (sqlite:connect (merge-pathnames dbname root-path)))
  (loop for path in (list #p"/usr/lib/sqlite3/pcre.so" #p"/usr/lib/sqlite3/pcre"
                          (merge-pathnames "data/sqlite3-pcre.so" (asdf:system-source-directory :radiance)))
        if (cl-fad:file-exists-p path)
          do (progn (load-extension *db* (namestring path))
                    (return T))
        finally (error 'pcre-not-found)))

(define-interface-method db:disconnect ()
  (v:info :sqlite "Disconnecting...")
  (sqlite:disconnect *db*)
  (setf *db* NIL))

(define-interface-method db:connected-p ()
  (not (null *db*)))

(define-interface-method db:collections ()
  (db:iterate "sqlite_master" (db::m-query :radiance-sqlite (:= "type" "table"))
    #'(lambda (row) (cdr (assoc "name" row :test #'string=)))
    :sort '(("name" . :ASC))))

(defun format-field-type (s arg colonp atp)
  (declare (ignore colonp atp))
  (if (listp arg)
      (case (length arg)
        (1 (format s "`~a`" (first arg)))
        (2 (format s "`~a` ~a" (first arg) (second arg)))
        (3 (format s "`~a` ~a(~a)" (first arg) (second arg) (third arg))))
      (format s "`~a`" arg)))

(define-interface-method db:create (collection fields &key indices (if-exists :ignore))
  (assert (not (null fields)) () "Fields cannot be an empty list!")
  (v:trace :sqlite "Creating collection ~a with fields ~a and indices ~a" collection fields indices)
  (sqlite:execute-non-query *db* (format NIL "CREATE TABLE ~:[~;IF NOT EXISTS ~]`~a` (_id INTEGER PRIMARY KEY AUTOINCREMENT, ~{~/radiance-mod-sqlite::format-field-type/~^, ~});" (eq if-exists :ignore) collection fields))
  (if indices
      (sqlite:execute-non-query *db* (format NIL "CREATE INDEX ~:[~;IF NOT EXISTS ~]`~a_idx` ON `~:*~a` (~{~a~^, ~})" (eq if-exists :ignore) collection indices)))) 
  
(define-interface-method db:empty (collection)
  (v:trace :sqlite "Emptying collection ~a" collection)
  (sqlite:execute-non-query *db* (format NIL "DELETE FROM `~a`" collection)))

(define-interface-method db:drop (collection)
  (v:trace :sqlite "Dropping collection ~a" collection)
  (sqlite:execute-non-query *db* (format NIL "DROP TABLE `~a`" collection)))

(define-interface-method db:select (collection query &key fields (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (v:trace :sqlite "Selecting data (~a) from ~a with query ~a, skipping ~a, limiting ~a, sorting by ~a" fields collection query skip limit sort)
    (get-data *db* (format NIL "SELECT ~a FROM `~a` ~a ~a ~:[~;~:*LIMIT ~D~] ~:[~;~:*OFFSET ~D~];" (fields-to-fields-part fields) collection where-part (sort-to-order-part sort) limit skip) values)))

(define-interface-method db:iterate (collection query function &key fields (skip 0) (limit -1) sort)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (v:trace :sqlite "Iterating data (~a) from ~a with query ~a, skipping ~a, limiting ~a, sorting by ~a" fields collection query skip limit sort)
    (get-data *db* (format NIL "SELECT ~a FROM `~a` ~a ~a ~:[~;~:*LIMIT ~D~] ~:[~;~:*OFFSET ~D~];" (fields-to-fields-part fields) collection where-part (sort-to-order-part sort) limit skip) values function)))

(define-interface-method db:insert (collection data)
  (multiple-value-bind (set-part values) (data-to-insert-part data)
    (v:trace :sqlite "Inserting into ~a: ~a" collection data)
    (db-query *db* (format NIL "INSERT INTO `~a` ~a;" collection set-part) values)))

(define-interface-method db:remove (collection query &key skip limit sort)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (v:trace :sqlite "Removing from ~a with query ~a, skipping ~a, limiting ~a, sorting by ~a" collection query skip limit sort)
    (if (or limit sort)
        (db-query *db* (format NIL "DELETE FROM `~a` WHERE `_id` = (SELECT `_id` FROM `~a`~a ~a ~:[~;~:*LIMIT ~D~] ~:[~;~:*OFFSET ~D~]);" collection collection where-part (sort-to-order-part sort) limit skip) values)
        (db-query *db* (format NIL "DELETE FROM `~a`~a ~a;" collection where-part (sort-to-order-part sort)) values))))

(define-interface-method db:update (collection query data &key (skip 0) (limit -1) sort replace)
  (multiple-value-bind (set-part values1) (data-to-set-part data)
    (multiple-value-bind (where-part values2) (query-to-where-part query)
      (v:trace :sqlite "Updating ~a with query ~a, skipping ~a, limiting ~a, sorting by ~a: ~a" collection query skip limit sort data)
      (if replace
          (progn
            (db-query *db* (format NIL "DELETE FROM `~a` WHERE `_id` = (SELECT `_id` FROM `~a`~a ~a ~:[~;~:*LIMIT ~D~] ~:[~;~:*OFFSET ~D~]);" collection collection where-part (sort-to-order-part sort) limit skip) values2)
            (db-query *db* (format NIL "INSERT INTO `~a` ~a;" collection set-part) values1))
          (if (or limit sort)
              (db-query *db* (format NIL "UPDATE `~a` ~a WHERE `_id` = (SELECT `_id` FROM `~a`~a ~a ~:[~;~:*LIMIT ~D~] ~:[~;~:*OFFSET ~D~]);" collection set-part collection where-part (sort-to-order-part sort) limit skip) (append values1 values2))
              (db-query *db* (format NIL "UPDATE `~a` ~a ~a ~a" collection set-part where-part (sort-to-order-part sort)) (append values1 values2)))))))

(define-interface-method db:apropos (collection)
  (v:trace :sqlite "Retrieving apropos of ~a" collection)
  (mapcar #'second (sqlite:execute-to-list *db* (format NIL "PRAGMA table_info(`~a`);" collection))))

(define-interface-method db:count (collection query)
  (multiple-value-bind (where-part values) (query-to-where-part query)
    (v:trace :sqlite "Counting data from ~a with query ~a" collection query)
    (cdaar (get-data *db* (format NIL "SELECT COUNT(*) FROM `~a` ~a" collection where-part) values))))

(defun query-to-where-part (query)
  (if (eq query :all) 
      (values "" ())
      (destructuring-bind (query . data) query
        (values (concatenate 'string "WHERE " query) data))))

(defun data-to-set-part (data)
  (loop for (key . val) in data
     collect key into keys
     collect val into vals
     finally (return (values (format NIL "SET ~{`~a` = ?~^, ~}" keys) vals))))

(defun data-to-insert-part (data)
  (loop for (key . val) in data
     collect key into keys
     collect val into vals
     finally (return (values (format NIL "(~{`~a`~^, ~}) VALUES (~{?~*~^, ~})" keys vals) vals))))

(defun format-order-by (s arg colonp atp)
  (declare (ignore colonp atp))
  (let ((sort (case (cdr arg)
                ((-1 :DESC) "DESC")
                ((1 :ASC) "ASC"))))
    (format s "`~a` ~a" (car arg) sort)))

(defun sort-to-order-part (sort)
  (if sort
      (format NIL "ORDER BY ~{~/radiance-mod-sqlite::format-order-by/~^, ~}" sort)
      ""))

(defun fields-to-fields-part (fields)
  (if fields 
      (etypecase fields
        (symbol (if (eq fields :all) "*" (symbol-name fields)))
        (list (format NIL "~{`~a`~^, ~}" fields)))
      "*"))

(defun get-data (db querystring queryargs &optional (function #'identity))
  (let ((stmt (sqlite:prepare-statement db querystring)))
    (loop for arg in queryargs
       for i from 1
       do (sqlite:bind-parameter stmt i arg))
    (loop while (sqlite:step-statement stmt)
       collect (funcall function
                        (loop for name in (sqlite:statement-column-names stmt)
                           for i from 0
                           collect (cons name (sqlite:statement-column-value stmt i))))
       finally (sqlite:finalize-statement stmt))))

(defun db-query (db querystring queryargs)
  (let ((stmt (sqlite:prepare-statement db querystring)))
    (loop for arg in queryargs
       for i from 1
       do (sqlite:bind-parameter stmt i arg))
    (sqlite:step-statement stmt)))
