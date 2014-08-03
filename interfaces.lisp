#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

;; As per spec
(define-interface (logger l)
  (defun log (level category log-string &rest format-args))
  (defun trace (category log-string &rest format-args))
  (defun debug (category log-string &rest format-args))
  (defun info (category log-string &rest format-args))
  (defun warn (category log-string &rest format-args))
  (defun error (category log-string &rest format-args))
  (defun severe (category log-string &rest format-args))
  (defun fatal (category log-string &rest format-args)))

;; As per spec
(define-interface (database db)
  (defun connect (database-name))
  (defun disconnect ())
  (defun connected-p ())
  (defun collections ())
  (defun create (collection structure &key indices (if-exists :ignore)))
  (defun structure (collection))
  (defun empty (collection))
  (defun drop (collection))
  (defun iterate (collection query function &key fields skip amount sort accumulate))
  (defun select (collection query &key fields skip amount sort))
  (defun count (collection query))
  (defun insert (collection data))
  (defun remove (collection query &key (skip 0) (amount 0) sort))
  (defun update (collection query data &key skip amount sort))
  (defmacro query (query-form)))

;; As per spec
(define-interface (data-model dm)
  (defclass data-model () ())
  (defield data-model)
  (defun id (data-model))
  (defun field (data-model field))
  (defun (setf field) (data-model field))
  (defun get (collection query &key (skip 0) (amount 0) sort))
  (defun get-one (collection query &key (skip 0) sort))
  (defun hull (collection))
  (defun hull-p (data-model))
  (defun save (data-model))
  (defun delete (data-model))
  (defun insert (data-model &key clone)))
