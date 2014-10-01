#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(define-trigger startup-done ()
  (db:connect (config-tree :sqlite :default))
  (load-pcre))

(define-trigger server-stop ()
  (db:disconnect))

(defun db:collections ()
  (db:iterate 'sqlite_master (db:query (:= 'type "table"))
    #'(lambda (row) (gethash "name" row))
    :sort '(("name" :ASC)) :accumulate T))

(defun db:collection-exists-p (collection)
  (ignore-errors
   (check-collection-exists collection)
   collection))

(defun db:create (collection structure &key indices (if-exists :ignore))
  (let ((collection (string-downcase collection)))
    (flet ((err (msg) (error 'database-invalid-collection :collection collection :message msg)))
      (check-collection-name collection)
      (unless structure (err "Structure cannot be empty."))
      (let ((query (format NIL "CREATE TABLE \"~a\" (\"_id\" INTEGER PRIMARY KEY AUTOINCREMENT, ~{~a~^, ~});"
                           collection (mapcar #'compile-field structure))))
        (when (db:collection-exists-p collection)
          (ecase if-exists
            (:ignore (return-from db:create NIL))
            (:error (error 'database-collection-already-exists :collection collection))))
        (exec-query query ())
        (dolist (index indices)
          (let ((index (if (listp index) index (list index))))
            (unless (every (member index `((_id) ,@structure) :key #'car :test #'string-equal) index)
              (err (format NIL "Index on field ~s requested but it does not exist." index))))
          (exec-query (format NIL "CREATE INDEX \"~a-~a\" ON \"~:*~:*~a\" (~{\"~(~a~)\"~^, ~})" collection index) ()))
        collection))))

(defun compile-field (field)
  (flet ((err (msg) (error 'database-invalid-field :fielddef field :message msg)))
    (destructuring-bind (name type) field
      (unless (valid-name-p name)
        (err "Invalid name, only a-z, - and _ are allowed."))
      (let ((arg (when (listp type) (prog1 (second type) (setf type (first type))))))
        (ecase type
          ((:INTEGER :ID)
           (format NIL "\"~a\" ~a" (string-downcase name)
                   (ecase arg ((1 2) "SMALLINT") ((3 4) "INTEGER") ((5 6 7 8) "BIGINT") ((NIL) "INTEGER"))))
          (:FLOAT
           (when arg (err "FLOAT cannot accept an argument."))
           (format NIL "\"~a\" DOUBLE PRECISION" (string-downcase name)))
          (:CHARACTER
           (error "CURRENTLY NOT SUPPORTED."))
          (:VARCHAR
           (unless arg (err "VARCHAR requires a length argument."))
           (format NIL "\"~a\" VARCHAR(~d)" (string-downcase name) arg))
          (:TEXT
           (when arg (err "TEXT cannot accept an argument."))
           (format NIL "\"~a\" TEXT" (string-downcase name))))))))

(defun db:structure (collection)
  (check-collection-exists collection)
  (cdr
   (loop for field in (sqlite:execute-to-list *current-con* (format NIL "PRAGMA table_info(\"~a\")" (string-downcase collection)))
	 collect (destructuring-bind (index name type &rest rest) field
		   (declare (ignore index rest))
		   (list name
			 (cond ((string-equal type "INTEGER")
				:INTEGER)
			       ((string-equal type "SMALLINT")
				(list :INTEGER 2))
			       ((string-equal type "BIGINT")
				(list :INTEGER 8))
			       ((string-equal type "DOUBLE PRECISION")
				:FLOAT)
			       ((string-equal type "TEXT")
				:TEXT)
			       ((string-equal type "VARCHAR" :end1 7)
				(list :VARCHAR (parse-integer (subseq type 8 (1- (length type))))))))))))

(defun db:empty (collection)
  (with-collection-existing (collection)
    (exec-query "TRUNCATE TABLE ?;" (list (string-downcase collection)))
    T))

(defun db:drop (collection)
  (with-collection-existing (collection)
    (exec-query "DROP TABLE ?;" (list (string-downcase collection)))
    T))

(defun collect-statement-to-table (statement)
  (loop with table = (make-hash-table :test 'equalp)
        for field in (sqlite:statement-column-names statement)
        for i from 0
        do (setf (gethash field table)
                 (sqlite:statement-column-value statement i))
        finally (return table)))

(defun collecting-iterator (function)
  #'(lambda (statement)
      (loop collect (funcall function (collect-statement-to-table statement))
	    while (sqlite:step-statement statement))))

(defun dropping-iterator (function)
  #'(lambda (statement)
      (loop do (funcall function (collect-statement-to-table statement))
	    while (sqlite:step-statement statement))))

(defun db:iterate (collection query function &key fields skip amount sort accumulate)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "SELECT ~:[*~;~:*~{\"~a\"~^ ~}~] FROM \"~a\"" (mapcar #'string-downcase fields) (string-downcase collection))
                             query skip amount sort) query vars)
      (exec-query query vars (if accumulate (collecting-iterator function) (dropping-iterator function))))))

(defun db:select (collection query &key fields skip amount sort)
  (db:iterate collection query #'identity :fields fields :skip skip :amount amount :sort sort :accumulate T))

(defun db:count (collection query)
  (with-collection-existing (collection)
    (with-query (query where vars)
      (let ((query (format NIL "SELECT COUNT(*) FROM \"~a\" ~a;" (string-downcase collection) where)))
        (exec-query query vars #'(lambda (statement)
				   (return-from db:count (sqlite:statement-column-value statement 0))))))))

(defun db:insert (collection data)
  (check-collection-name collection)
  (with-collection-existing (collection)
    (let ((query (format NIL "INSERT INTO \"~a\" (~~{\"~~a\"~~^, ~~}) VALUES (~~:*~~{~~*?~~^, ~~});" (string-downcase collection))))
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          collect (string-downcase field) into fields
                          collect value into values
                          finally (exec-query (format NIL query fields) values))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data))))
      (sqlite:last-insert-rowid *current-con*))))

(defun db:remove (collection query &key skip amount sort)
  (check-collection-name collection)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "DELETE FROM \"~a\" WHERE \"_id\" IN (SELECT \"_id\" FROM \"~:*~a\" " (string-downcase collection))
                             query skip amount sort) query vars)
      (exec-query (format NIL "~a );" query) vars)
      T)))

(defun db:update (collection query data &key skip amount sort)
  (check-collection-name collection)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "UPDATE \"~a\" SET ~~{\"~~a\" = ?~~^, ~~} WHERE \"_id\" IN (SELECT \"_id\" FROM \"~:*~a\" "
                                     (string-downcase collection))
                             query skip amount sort) query vars)
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          collect value into values
                          collect (string-downcase field) into fields
                          finally (exec-query (format NIL "~a);" (format NIL query fields)) (append vars values)))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data))))
      T)))
