#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(defmacro with-query ((query-form &optional (where 'where) (vars 'vars)) &body body)
  (let ((res (gensym "RESULT")))
    `(let* ((,res ,query-form)
            (,where (car ,res))
            (,vars (cdr ,res)))
       ,@body)))

(defun exec-query (query vars &optional (row-reader #'(lambda (statement) (declare (ignore statement)))))
  (l:trace :database "QUERY: ~s ~s" query vars)
  (let ((statement (sqlite:prepare-statement *current-con* query)))
    (loop for i from 1
	  for var in vars
	  do (sqlite:bind-parameter statement i var))
    (unwind-protect
	 (progn
	   (sqlite:step-statement statement)
	   (funcall row-reader statement))
      (sqlite:finalize-statement statement))))

(defun %sort-clause (s a c p)
  (declare (ignore c p))
  (let ((field (first a))
        (order (second a)))
    (ecase order (:DESC) (:ASC))
    (format s "\"~a\" ~a" (string-downcase field) order)))

(defun make-query (base where skip amount sort)
  (assert (or (null amount) (integerp amount)))
  (assert (or (null skip) (integerp skip)))
  (assert (listp sort))
  (with-query (where where vars)
    (cons (format NIL "~a ~a~@[ ORDER BY ~{~/i-sqlite::%sort-clause/~^, ~}~]~@[ LIMIT ~d~]~:[~; OFFSET ~d~]"
                  base where sort amount (and skip (/= skip 0)) skip)
          vars)))

(defvar *vars*)
(defun compile-form (form)
  (etypecase form
    (null (error "NIL not allowed"))
    (symbol
     (push form *vars*)
     "?")
    (real form)
    (character form)
    (string
     (format NIL "'~a'" form))
    (list
     (case (first form)
       (:= (format NIL "(~a) = (~a)" (compile-form (second form)) (compile-form (third form))))
       (:!= (format NIL "(~a) != (~a)" (compile-form (second form)) (compile-form (third form))))
       (:> (format NIL "(~a) > (~a)" (compile-form (second form)) (compile-form (third form))))
       (:< (format NIL "(~a) < (~a)" (compile-form (second form)) (compile-form (third form))))
       (:<= (format NIL "(~a) <= (~a)" (compile-form (second form)) (compile-form (third form))))
       (:>= (format NIL "(~a) >= (~a)" (compile-form (second form)) (compile-form (third form))))
       (:MATCHES (format NIL "(~a) REGEXP (~a)" (compile-form (second form)) (compile-form (third form))))
       (:IN (format NIL "(~a) IN (~{~a~^, ~})" (compile-form (second form)) (mapcar #'compile-form (cddr form))))
       (:AND (format NIL "~{(~a)~^ AND ~}" (mapcar #'compile-form (rest form))))
       (:OR (format NIL "~{(~a)~^ OR ~}" (mapcar #'compile-form (rest form))))
       (:NOT (format NIL "NOT (~a)" (compile-form (second form))))
       (:FIELD (format NIL "\"~a\"" (string-downcase (second form))))
       (QUOTE (format NIL "\"~a\"" (string-downcase (second form))))
       (T (push form *vars*)
	"?")))))

(defmacro db:query (query-form)
  (let ((*vars* ()))
    (if (eql query-form :ALL)
        `(cons "" ())
        `(cons ,(format NIL "WHERE ~a" (compile-form query-form))
               (list ,@(nreverse *vars*))))))
