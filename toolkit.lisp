#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defun read-value ()
  (eval (read)))

(defun %static-file (namestring base)
  (merge-pathnames namestring (merge-pathnames "static/" (merge-pathnames (resolve-base base)))))

(defun static-file (namestring &optional base)
  (%static-file namestring base))

(define-compiler-macro static-file (namestring &optional (base *package*))
  (typecase base
    ((or package string)
     (if (stringp namestring)
         (%static-file namestring base)
         `(merge-pathnames ,namestring ,(merge-pathnames "static/" (merge-pathnames (resolve-base base))))))
    (T `(%static-file ,namestring ,base))))

(defun %template (namestring base)
  (merge-pathnames namestring (merge-pathnames "template/" (merge-pathnames (resolve-base base)))))

(defun template (namestring &optional base)
  (%template namestring base))

(define-compiler-macro template (namestring &optional (base *package*))
  (typecase base
    ((or package string)
     (if (stringp namestring)
         (%template namestring base)
         `(merge-pathnames ,namestring ,(merge-pathnames "template/" (merge-pathnames (resolve-base base))))))
    (T `(template ,namestring ,base))))

(defmacro with-model-fields (object fields &body body)
  (let ((model (gensym "MODEL")))
    `(let ((,model ,object))
       (declare (ignorable ,model))
       (symbol-macrolet
           ,(mapcar #'(lambda (field)
                        (destructuring-bind (name &optional (field name)) (if (listp field) field (list field))
                          `(,name (dm:field ,model ,(string-downcase field))))) fields)
         ,@body))))

(defmacro with-model (modelvar (collection query &rest fields) &body body)
  `(let ((,modelvar ,(if query
                         `(dm:get-one ,collection ,query)
                         `(dm:hull ,collection))))
     (declare (ignorable ,modelvar))
     ,@(if fields
           `((with-model-fields ,modelvar ,fields
               ,@body))
           body)))

(defmacro do-models (modelvar (collection query &rest fields) &body body)
  `(dolist (,modelvar (dm:get ,collection ,query))
     (with-model-fields ,modelvar ,fields
       ,@body)))

(defmacro with-actions ((error info) action-clauses &body body)
  (let ((action (gensym "ACTION")))
    `(let ((,error) (,info)
           (,action (post/get "action")))
       (declare (ignorable ,error ,info))
       (handler-case
           (cond
             ,@(loop for (clause . body) in action-clauses
                     collect `((string-equal ,action ,(string clause)) ,@body)))
         (,error (err)
           (setf ,error err)))
       ,@body)))

(indent:define-indentation with-actions (6 (&whole 4 &rest) &body))

(defun cut-get-part (url)
  (subseq url 0 (position #\? url)))

(defun or* (&rest vals)
  "Functional equivalent of OR with the twist that empty strings are seen as NIL."
  (loop for val in vals
        when (if (stringp val)
                 (not (string= val ""))
                 val)
        do (return val)))

(defun format-universal-time (ut)
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~}"
          (subseq (nreverse (multiple-value-list (decode-universal-time ut))) 3)))
