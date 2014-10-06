#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defun lambda-keyword-p (symbol)
  "Returns the symbol if it is a lambda-keyword symbol (the &-options)."
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun remove-aux-part (lambda-list)
  "Removes the &aux part of the lambda-list."
  (let ((position (position '&aux lambda-list)))
    (if position
        (subseq lambda-list 0 position)
        lambda-list)))

(defun flatten-lambda-list (lambda-list)
  "Flattens the lambda-list by replacing all lists within it with their respective first symbol."
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  "Extracts the symbols that name the variables in the lambda-list."
  (remove-if #'lambda-keyword-p (flatten-lambda-list (remove-aux-part lambda-list))))

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

(defun or* (&rest vals)
  "Functional equivalent of OR with the twist that empty strings are seen as NIL."
  (loop for val in vals
        when (if (stringp val)
                 (not (string= val ""))
                 val)
        do (return val)))

(defun cut-get-part (url)
  (subseq url 0 (position #\? url)))
