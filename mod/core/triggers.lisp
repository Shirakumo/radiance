#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.radiance.mod.core)

(defvar *radiance-hooks* (make-hash-table) "Map of all registered hooks.")

(defclass hook ()
  ((name :initform (error "Hook name required.") :initarg :name :accessor name :type symbol)
   (module :initform (error "Hook target module required.") :initarg :module :accessor module :type module)
   (function :initform (error "Hook module method required.") :initarg :function :accessor hook-function :type function)
   (description :initform NIL :initarg :description :accessor description :type string))
  (:documentation "Radiance hook class"))

(defun defhook (name module function &key description)
  "Defines a new hook of name, for a certain function of a module."
  (setf (gethash name *radiance-triggers*)
        (append (gethash name *radiance-triggers*)
                (list (make-instance 'hook :name name :module module :function function :description description)))))

(defun trigger (trigger &rest args)
  "Trigger a certain hook and collect all return values."
  (loop for hook in (gethash trigger *radiance-triggers*)
       collect (apply (hook-function hook) (module hook) args)))
