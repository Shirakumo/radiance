#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-postmodern)

(defmacro with-query ((query-form &optional (where 'where) (vars 'vars)) &body body)
  (let ((res (gensym "RESULT")))
    `(let* ((,res ,query-form)
            (,where (car ,res))
            (,vars (cdr ,res)))
       ,@body)))

(defmacro with-collection-existing ((collection) &body body)
  `(handler-case (progn ,@body)
     (cl-postgres-error:syntax-error-or-access-violation (err)
       (when (string= "42P01" (cl-postgres-error::database-error-code err))
         (error 'database-invalid-collection :collection ,collection
                                             :message (cl-postgres-error::database-error-message err))))))

(defun valid-name-p (name)
  (loop for char across (string-downcase name)
        always (or (alpha-char-p char) (char= char #\_) (char= char #\-))))

(defun check-collection-name (collection)
  (unless (valid-name-p collection)
    (error 'database-invalid-collection :collection collection :message "Invalid name, only a-z, - and _ are allowed.")))

(defun check-collection-exists (collection)
  (check-collection-name collection)
  (with-connection
    (unless (postmodern:table-exists-p (string-downcase collection))
      (error 'database-invalid-collection :collection collection :message "Collection does not exist on database."))))
