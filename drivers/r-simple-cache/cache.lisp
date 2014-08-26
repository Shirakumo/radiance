#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module simple-cache
  (:use #:cl #:radiance)
  (:implements #:cache))
(in-package #:simple-cache)

(defvar *cache-directory* (data-file "simple-cache/"))
(defvar *caches* (make-hash-table :test 'eql))

(define-trigger startup-done ()
  (ensure-directories-exist *cache-directory*))

(define-trigger shutdown-done ()
  (uiop:delete-directory-tree *cache-directory* :validate (constantly T)))

(defun (setf cache::builder) (builder-func name)
  (setf (gethash name *caches*) builder-func))

(defun cache::builder (name)
  (gethash name *caches*))

(defun cache::file (symbol)
  (merge-pathnames (format NIL "~a/~a" (package-name (symbol-package symbol)) (symbol-name symbol)) *cache-directory*))

(defun cache:get (name)
  (read-data-file (cache::file name)))

(defun cache:renew (name)
  (delete-file (cache::file name)))

(defun cache::exists (name)
  (probe-file (cache::file name)))

(defun cache::output (name result)
  (let ((file (cache::file name)))
    (ensure-directories-exist file)
    (etypecase result
      (string
       (with-open-file (stream file :direction :output :if-exists :supersede)
         (write-string result stream)))
      ((array (unsigned-byte 8))
       (with-open-file (stream file :direction :output :if-exists :supersede :element-type '(array (unsigned-byte 8)))
         (write-sequence result stream)))))
  result)

(defmacro cache:with-cache (name test &body request-generator)
  (let ((cache (gensym "CACHE")))
    `(let ((,cache ,name))
       (if (or (not (cache::exists ,cache))
               ,test)
           (cache::output ,cache ((lambda () ,@request-generator)))
           (cache:get ,cache)))))
