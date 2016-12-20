#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.test)

(define-test database
  :parent interfaces)

(define-test connection
  :parent database
  (fail (db:connect "this-db-should-not-exist") 'db:connection-failed)
  (finish (db:connect "test"))
  (fail (db:connect "test") 'db:connection-already-open)
  (true (db:connected-p))
  (finish (db:disconnect)))

(defmacro with-clean-database ((name) &body body)
  `(progn
     (db:connect ,name)
     (unwind-protect
          (progn
            (dolist (collection (db:collections))
              (db:drop collection))
            ,@body)
       (db:disconnect))))

(define-test collection
  :parent database
  :depends-on (connection)
  (with-clean-database ("test")
    (is equal () (db:collections))
    (false (db:collection-exists-p "test"))
    (fail (db:create ":" ()) 'db:invalid-collection)
    (fail (db:create "test" '((|:| :int)) 'db:invalid-field))
    (fail (db:create "test" '((foo :foo))) 'db:invalid-field)
    (true (db:create "test" '((id :id)
                              (integer :integer)
                              (integer1 (:integer 1))
                              (integer2 (:integer 2))
                              (integer3 (:integer 3))
                              (integer4 (:integer 4))
                              (integer5 (:integer 5))
                              (integer6 (:integer 6))
                              (integer7 (:integer 7))
                              (integer8 (:integer 8))
                              (float :float)
                              (character :character)
                              (varchar (:varchar 32))
                              (text :text))))
    (false (db:create "test" '()))
    (true (db:collection-exists-p "test"))
    (fail (db:create "test" '() :if-exists :error) 'db:collection-already-exists)
    (is equal '("test") (db:collections))
    (true (db:structure "test"))
    (finish (db:drop "test"))
    (false (db:collection-exists-p "test"))
    (fail (db:drop "test") 'db:invalid-collection)
    (is equal () (db:collections))))

(define-test record
  :parent database
  :depends-on (collection)
  (with-clean-database ("test")
    (db:create "test" '((number :integer)
                        (name (:varchar 32))
                        (text :text)))
    ;; Test insertion
    (of-type db:id (db:insert "test" '((number . 0)
                                       (name . "Guybrush")
                                       (text . "Can hold his breath for ten minutes."))))
    (is = 1 (db:count "test" (db:query :all)))
    ;; Test ID coercion
    (let ((id (db:insert "test" (alexandria:alist-hash-table
                                 '((number . 1)
                                   (name . "Nicolas Hafner")
                                   (text . "Someone insignificant."))))))
      (is = 2 (db:count "test" (db:query :all)))
      (of-type db:id (db:ensure-id (princ-to-string id))))
    ;; Test count and primitive query
    (dotimes (i 100)
      (of-type db:id (db:insert "test" `((number . ,(+ 5 i))
                                         (name . "Tester")))))
    (is = 102 (db:count "test" (db:query :all)))
    (is = 100 (db:count "test" (db:query (:= 'name "Tester"))))
    (is =  50 (db:count "test" (db:query (:< 'number 50))))
    ;; Test emptying
    (finish (db:empty "test"))
    (is = 0 (db:count "test" (db:query :all)))
    ;; Test removal
    (dotimes (i 100)
      (of-type db:id (db:insert "test" `((number . ,i)
                                         (name . "Tester")))))
    (is = 100 (db:count "test" (db:query :all)))
    (finish (db:remove "test" (db:query (:= 'number 50))))
    (is =  99 (db:count "test" (db:query :all)))
    (is =   0 (db:count "test" (db:query (:= 'number 50))))
    ;; Test update
    (finish (db:update "test" (db:query (:= 'number 99)) '((number . 50))))
    (is =  99 (db:count "test" (db:query :all)))
    (is =   1 (db:count "test" (db:query (:= 'numer 50))))
    ;; Test selection
    (is =  99 (length (db:select "test" (db:query :all))))
    (dolist (record (db:select "test" (db:query :all)))
      (is equal "Tester" (gethash record "name")))
    (loop for record in (db:select "test" (db:query :all) :sort '((number :asc)))
          for i from 0 below 99
          do (is = i (gethash "number" record)))
    (loop for record in (db:select "test" (db:query :all) :sort '((number :asc)) :skip 5)
          for i from 5 below 99
          do (is = i (gethash "number" record)))
    (loop for record in (db:select "test" (db:query :all) :sort '((number :asc)) :amount 5)
          for i from 0 below 5
          do (is = i (gethash "number" record)))
    ;; Test iteration
    (db:iterate "test" (db:query :all)
                (lambda (record)
                  (of-type (integer 0 98) (gethash "number" record))
                  (is equal "Tester" (gethash "name" record))
                  (false (gethash "text" record))))
    (is equal
        (loop repeat 99 collect "Tester")
        (db:iterate "test" (db:query :all) (lambda (r) (gethash "name" r)) :accumulate T))
    (is =
        (loop for i from 0 below 99 sum i)
        (reduce #'+ (db:iterate "test" (db:query :all) (lambda (r) (gethash "number" r)) :accumulate T)))
    ;; Test update (again)
    (finish (db:update "test" (db:query (:< 'number 50)) '(("name" . "retseT"))))
    (is = 50 (db:count "test" (db:query (:= 'name "retseT"))))
    ;; Test removal (again)
    (finish (db:remove "test" (db:query (:= 'name "retseT")) :limit 5))
    (is = 45 (db:count "test" (db:query (:= 'name "retseT"))))))

;; (define-test transactions
;;   :parent database
;;   :depends-on (record)
;;   (with-clean-database ("test")
;;     ))
