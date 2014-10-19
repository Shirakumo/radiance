#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defmacro define-options-definer (name setter args)
  (let ((namevar (gensym "NAME"))
        (valuevar (car (last args)))
        (common-args (butlast args)))
    `(defmacro ,name (,namevar ,args &body body)
       `(eval-when (:compile-toplevel :load-toplevel :execute)
          (setf (,',setter ,(make-keyword ,namevar))
                #'(lambda (,,@common-args &optional ,,valuevar)
                    (declare (ignorable ,,@common-args))
                    ,@body))))))

(defun expand-options (options-table options body &rest common-args)
  (let ((no-value (gensym "NO-VALUE")))
    (loop with result = NIL
          for option being the hash-keys of options-table
          for function being the hash-values of options-table
          for value = (getf options option no-value)
          do (multiple-value-bind (new-body forms)
                 (if (eql value no-value)
                     (apply function (append common-args (list body)))
                     (apply function (append common-args (list body value))))
               (setf body new-body
                     result forms))
          when result
            collect result into results
          finally (return (values body results)))))
