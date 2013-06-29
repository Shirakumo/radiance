#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-triggers* (make-hash-table) "Map of all registered triggers.")

(defclass hook ()
  ((name :initform (error "Hook name required.") :initarg :name :accessor name :type symbol)
   (module :initform (error "Hook target module required.") :initarg :module :accessor module :type module)
   (function :initform (error "Hook module method required.") :initarg :function :accessor hook-function :type function)
   (description :initform NIL :initarg :description :accessor description :type string))
  (:documentation "Radiance hook class"))

(defmethod print-object ((hook hook) out)
  (print-unreadable-object (hook out :type T)
    (format out "~a -> ~a:~a" (name hook) (module hook) (hook-function hook))))

(defun defhook (name module function &key description)
  "Defines a new hook of name, for a certain function of a module."
  (let ((instance (make-instance 'hook :name name :module module :function function :description description)))
    (setf (gethash name *radiance-triggers*)
          (append (gethash name *radiance-triggers*)
                  (list instance)))
    instance))

(defun trigger (trigger &rest args)
  "Trigger a certain hook and collect all return values."
  (loop for hook in (gethash trigger *radiance-triggers*)
       collect (apply (hook-function hook) (module hook) args)))
