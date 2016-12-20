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
    (dotimes (i 10)
      (of-type db:id (db:insert "test" `((number . ,i)
                                         (name . "Tester")))))
    (is = 12 (db:count "test" (db:query :all)))
    (is = 10 (db:count "test" (db:query (:= 'name "Tester"))))
    (is =  7 (db:count "test" (db:query (:< 'number 5))))
    ;; Test emptying
    (finish (db:empty "test"))
    (is = 0 (db:count "test" (db:query :all)))
    ;; Test removal
    (dotimes (i 10)
      (of-type db:id (db:insert "test" `((number . ,i)
                                         (name . "Tester")))))
    (is = 10 (db:count "test" (db:query :all)))
    (finish (db:remove "test" (db:query (:= 'number 5))))
    (is =  9 (db:count "test" (db:query :all)))
    (is =  0 (db:count "test" (db:query (:= 'number 5))))
    ;; Test update
    (finish (db:update "test" (db:query (:= 'number 9)) '((number . 5))))
    (is =  9 (db:count "test" (db:query :all)))
    (is =  1 (db:count "test" (db:query (:= 'number 5))))
    ;; Test selection
    (is =  9 (length (db:select "test" (db:query :all))))
    (dolist (record (db:select "test" (db:query :all)))
      (is equal "Tester" (gethash "name" record)))
    (loop for record in (db:select "test" (db:query :all) :sort '((number :asc)))
          for i from 0 below 9
          do (is = i (gethash "number" record)))
    (loop for record in (db:select "test" (db:query :all) :sort '((number :asc)) :skip 5)
          for i from 5 below 9
          do (is = i (gethash "number" record)))
    (loop for record in (db:select "test" (db:query :all) :sort '((number :asc)) :amount 5)
          for i from 0 below 5
          do (is = i (gethash "number" record)))
    ;; Test iteration
    (db:iterate "test" (db:query :all)
                (lambda (record)
                  (of-type (integer 0 8) (gethash "number" record))
                  (is equal "Tester" (gethash "name" record))
                  (false (gethash "text" record))))
    (is equal
        (loop repeat 9 collect "Tester")
        (db:iterate "test" (db:query :all) (lambda (r) (gethash "name" r)) :accumulate T))
    (is =
        (loop for i from 0 below 9 sum i)
        (reduce #'+ (db:iterate "test" (db:query :all) (lambda (r) (gethash "number" r)) :accumulate T)))
    ;; Test update (again)
    (finish (db:update "test" (db:query (:< 'number 5)) '(("name" . "retseT"))))
    (is = 5 (db:count "test" (db:query (:= 'name "retseT"))))
    ;; Test removal (again)
    (finish (db:remove "test" (db:query (:= 'name "retseT")) :amount 2))
    (is = 3 (db:count "test" (db:query (:= 'name "retseT"))))
    ;; Test ID (again)
    (let ((id (db:insert "test" '((number . 1)
                                  (name . "foo")
                                  (text . "bar")))))
      (is = 1 (gethash "number" (first (db:select "test" (db:query (:= '_id id))))))
      (is equal "foo" (gethash "name" (first (db:select "test" (db:query (:= '_id id))))))
      (is equal "bar" (gethash "text" (first (db:select "test" (db:query (:= '_id id)))))))))

(define-test transactions
  :parent database
  :depends-on (record)
  (with-clean-database ("test")
    (db:create "test" '((number :integer)))
    (finish (db:with-transaction ()
              (db:insert "test" '((number . 0)))))
    (is = 1 (db:count "test" (db:query :all)))
    (fail (db:with-transaction ()
            (db:insert "test" '((number . 1)))
            (error "Exit transaction")))
    (is = 1 (db:count "test" (db:query :all)))))

(define-test benchmark
  :parent database
  :depends-on (record)
  (with-clean-database ("test")
    (db:create "test" '((number :integer)))
    (finish (dotimes (i 1000)
              (db:insert "test" '((number . 0)))))
    (finish (db:count "test" (db:query :all)))
    (finish (db:select "test" (db:query (:= 'number 1))))
    (finish (db:remove "test" (db:query :all) :amount 500))))
