#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-postmodern)

;; TODO:
;; Optimize the shit out of this with compiler macros
;; maybe even triggers to compile all prepared statements on db connect or some shit

;; TODO:
;; Implement closer erroring details of spec. (invalid fields, etc)

(define-trigger startup-done ()
  (db:connect (config-tree :postmodern :default)))

(define-trigger server-stop ()
  (db:disconnect))

(defun db:collections ()
  (with-connection
    (postmodern:list-tables T)))

(defun db:collection-exists-p (collection)
  (with-connection
    (postmodern:table-exists-p (string-downcase collection))))

(defun db:create (collection structure &key indices (if-exists :ignore))
  (flet ((err (msg) (error 'database-invalid-collection :collection collection :message msg)))
    (check-collection-name collection)
    (unless structure (err "Structure cannot be empty."))
    (let ((query (format NIL "CREATE TABLE \"~a\" (\"_id\" INTEGER PRIMARY KEY DEFAULT nextval('~:*~a-id-seq'), ~{~a~^, ~});"
                         (string-downcase collection) (mapcar #'compile-field structure))))
      (with-connection
        (when (postmodern:table-exists-p (string-downcase collection))
          (ecase if-exists
            (:ignore (return-from db:create NIL))
            (:error (error 'database-collection-already-exists :collection collection))))
        (postmodern:query (format NIL "CREATE SEQUENCE \"~a-id-seq\";" (string-downcase collection)))
        (postmodern:query query)
        (postmodern:query (format NIL "CREATE INDEX ON \"~a\" (\"_id\")" (string-downcase collection)))
        (dolist (index indices)
          (let ((index (if (listp index) index (list index))))
            (unless (every (member index `((_id) ,@structure) :key #'car :test #'string-equal) index)
              (err (format NIL "Index on field ~s requested but it does not exist." index)))
            (postmodern:query (format NIL "CREATE INDEX ON \"~a\" (~{\"~(~a~)\"~^, ~})"
                                      (string-downcase collection) index)))))
      T)))

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
  (with-connection
    (rest 
     (mapcar #'(lambda (column)
                 (destructuring-bind (name type size) column
                   (list name (cond ((string= type "integer")
                                     :INTEGER)
                                    ((string= type "smallint")
                                     (list :INTEGER 2))
                                    ((string= type "bigint")
                                     (list :INTEGER 8))
                                    ((string= type "double precision")
                                     :FLOAT)
                                    ((string= type "character varying")
                                     (list :VARCHAR size))
                                    ((string= type "text")
                                     :TEXT)))))
             (postmodern:query (format NIL "select column_name, data_type, character_maximum_length from INFORMATION_SCHEMA.COLUMNS where TABLE_NAME = '~a'"
                                       (string-downcase collection)))))))

(defun db:empty (collection)
  (with-collection-existing (collection)
    (with-connection
      (postmodern:query (format NIL "TRUNCATE TABLE \"~a\" CASCADE;" (string-downcase collection)))
      T)))

(defun db:drop (collection)
  (with-collection-existing (collection)
    (with-connection
      (postmodern:query (format NIL "DROP TABLE \"~a\" CASCADE;" (string-downcase collection)))
      (postmodern:query (format NIL "DROP SEQUENCE \"~a-id-seq\" CASCADE;" (string-downcase collection)))
      T)))

(defun collecting-iterator (function)
  (cl-postgres:row-reader (fields)
    (loop while (cl-postgres:next-row)
          for table = (let ((table (make-hash-table :test 'equalp)))
                        (loop for field across fields
                              do (setf (gethash (cl-postgres:field-name field) table) (cl-postgres:next-field field))) table)
          collect (funcall function table))))

(defun dropping-iterator (function)
  (cl-postgres:row-reader (fields)
    (loop while (cl-postgres:next-row)
          for table = (let ((table (make-hash-table :test 'equalp)))
                        (loop for field across fields
                              do (setf (gethash (cl-postgres:field-name field) table) (cl-postgres:next-field field))) table)
          do (funcall function table))))

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
      (let ((query (format NIL "SELECT COUNT(*) AS c FROM \"~a\" ~a;" (string-downcase collection) where)))
        (car (exec-query query vars (collecting-iterator #'(lambda (ta) (gethash "c" ta)))))))))

(defun db:insert (collection data)
  (check-collection-name collection)
  (with-collection-existing (collection)
    (let ((query (format NIL "INSERT INTO \"~a\" (~~{\"~~a\"~~^, ~~}) VALUES (~~{$~~a~~^, ~~}) RETURNING \"_id\";" (string-downcase collection))))
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters 
                          for i from 1
                          collect (string-downcase field) into fields
                          collect value into values
                          collect i into nums
                          finally (return (car (exec-query (format NIL query fields nums) values
                                                           (collecting-iterator #'(lambda (ta) (gethash "_id" ta)))))))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data)))))))

(defun db:remove (collection query &key skip amount sort)
  (check-collection-name collection)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "DELETE FROM \"~a\" WHERE ctid IN (SELECT ctid FROM \"~:*~a\" " (string-downcase collection))
                             query skip amount sort) query vars)
      (exec-query (format NIL "~a );" query) vars)
      T)))

(defun %field-clause (s a c p)
  (declare (ignore c p))
  (format s "\"~a\" = $~a" (car a) (cdr a)))

(defun db:update (collection query data &key skip amount sort)
  (check-collection-name collection)
  (with-collection-existing (collection)
    (with-query ((make-query (format NIL "UPDATE \"~a\" SET ~~{~~/i-postmodern::%field-clause/~~^, ~~} WHERE ctid IN (SELECT ctid FROM \"~:*~a\" "
                                     (string-downcase collection))
                             query skip amount sort) query vars)
      (macrolet ((looper (&rest iters)
                   `(loop ,@iters
                          for i from (1+ (length vars))
                          collect value into values
                          collect (cons (string-downcase field) i) into fields
                          finally (exec-query (format NIL "~a );" (format NIL query fields)) (append vars values)))))
        (etypecase data
          (hash-table
           (looper for field being the hash-keys of data
                   for value being the hash-values of data))
          (list
           (looper for (field . value) in data))))
      T)))
