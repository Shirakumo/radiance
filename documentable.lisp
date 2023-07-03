(in-package #:org.shirakumo.radiance.core)

(defclass documentable ()
  ((documentation :initarg :documentation))
  (:default-initargs
   :documentation NIL))

(defmethod documentation ((documentable documentable) type)
  (slot-value documentable 'documentation))

(defmethod (setf documentation) (string (documentable documentable) type)
  (setf (slot-value documentable 'documentation) string))

(defmacro define-documentable (name direct-superclasses direct-slots &rest options)
  (let ((find-function (second (find :find-function options :key #'first))))
    `(progn
       (defclass ,name (,@direct-superclasses documentable)
         ,direct-slots
         ,@(remove :find-function options :key #'first))
       ,@(when find-function
           `((defmethod documentation ((name symbol) (type (eql ',name)))
               (documentation (,find-function name) T))
             (defmethod (setf documentation) (string (name symbol) (type (eql ',name)))
               (setf (documentation (,find-function name) T) string))
             (defmethod documentation ((name cons) (type (eql ',name)))
               (documentation (,find-function name) T))
             (defmethod (setf documentation) (string (name cons) (type (eql ',name)))
               (setf (documentation (,find-function name) T) string))
             (docs:define-documentation-test ,name (name)
               (,find-function name)))))))
