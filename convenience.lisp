#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

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

