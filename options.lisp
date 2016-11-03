#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *options* (make-hash-table :test 'eql))

(define-documentable option ()
  ((type :initarg :type :accessor option-type)
   (name :initarg :name :accessor name)
   (expander :initarg :expander :accessor expander))
  (:default-initargs
   :type (error "TYPE required.")
   :name (error "NAME required.")
   :expander (error "EXPANDER required."))
  (:find-function option))

(defmethod print-object ((option option) stream)
  (print-unreadable-object (option stream :type T)
    (format stream "~a ~s" (option-type option) (name option))))

(defun option (type name)
  (let ((options (gethash type *options*)))
    (when options (gethash name options))))

(defun (setf option) (option type name)
  (let ((options (or (gethash type *options*)
                     (setf (gethash type *options*)
                           (make-hash-table :test 'eql)))))
    (setf (gethash name options) option)))

(defun remove-option (type name)
  (let ((options (gethash type *options*)))
    (when options (remhash name options))))

(defun list-options (type)
  (let ((options (gethash type *options*)))
    (when options
      (loop for option being the hash-values of options
            collect option))))

(defmacro define-option (type name args &body body)
  (let ((name (intern (string name) :keyword))
        (handler (gensym "HANDLER")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (flet ((,handler ,args
                ,@body))
         (setf (option ',type ,name)
               (make-instance 'option
                              :type ',type
                              :name ,name
                              :expander #',handler))))))

(defun expand-options (type options name body &rest args)
  (let ((no-value (gensym "NO-VALUE")))
    (loop with forms = ()
          for option in (list-options type)
          for value = (getf options (name option) no-value)
          do (multiple-value-bind (new-body new-form)
                 (if (eql value no-value)
                     (apply (expander option) name body args)
                     (apply (expander option) name body (append args (list value))))
               (setf body new-body)
               (push new-form forms))
          finally (return (values body forms)))))
