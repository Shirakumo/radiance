#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

;; !ADAPT
(defmacro with-collection-existing ((collection) &body body)
  `(handler-bind
       ((sqlite:sqlite-error
          #'(lambda (err)
              (when (and (<= 13 (length (sqlite:sqlite-error-message err)))
                         (string= "no such table" (sqlite:sqlite-error-message err) :end2 13))
                (error 'database-invalid-collection :collection ,collection
                                                    :message (sqlite:sqlite-error-message err))))))
     ,@body))

(defun valid-name-p (name)
  (loop for char across (string-downcase name)
        always (or (alpha-char-p char) (char= char #\_) (char= char #\-))))

(defun check-collection-name (collection)
  (unless (valid-name-p collection)
    (error 'database-invalid-collection :collection collection :message "Invalid name, only a-z, - and _ are allowed.")))

(defun check-collection-exists (collection)
  (check-collection-name collection)
  (when (= 0 (db:count 'sqlite_master (db:query (:and (:= 'type "table") (:= 'name (string-downcase collection))))))
    (error 'database-invalid-collection :collection collection :message "Collection does not exist on database.")))
