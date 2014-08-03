#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defvar *page-options* (make-hash-table))

(defun page-option (name)
  (gethash name *page-options*))

(defun (setf page-option) (option name)
  (setf (gethash name *page-options*) option))

(defmacro define-page-option (name (namevar urivar &rest rest) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (page-option ,(make-keyword name))
           #'(lambda (,namevar ,urivar ,@rest)
               ,@body))))

(defvar *page-body*)
(defmacro define-page (name uri options &body body)
  (let ((*page-body* body)
        (no-value (gensym "NO-VALUE")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop for option being the hash-keys of *page-options*
               for function being the hash-values of *page-options*
               for value = (getf options option no-value)
               for result = (if (eql value no-value)
                                (funcall function name uri)
                                (funcall function name uri value))
               when result
                 collect result)
       (define-uri-dispatcher ,name (,uri ,(gensym "REQUEST"))
         ,@*page-body*))))
