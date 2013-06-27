#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :org.tymoonnext.radiance.mod.core)

(defvar *radiance-implements* (make-hash-table) "Radiance implements table.")

(defclass implementation ()
  ((module :initform NIL :initarg :module :reader module :type module)
   (superclass :initform (error "Superclass required.") :initarg :superclass :accessor superclass :type symbol))
  (:documentation "Radiance implementation class to hold information about an implementations slot."))

(defgeneric implement (slot module)
  (:documentation "Registers a module for an implementation."))

(defmacro defimpl (slot superclass)
  "Defines an implementations interface."
  `(progn 
     (setf (gethash ',slot *radiance-implements*)
           (make-instance 'implementation :superclass ',superclass))
     (defmethod implement ((slot (eql ',slot)) (module ,superclass))
       (setf (superclass (gethash ',slot *radiance-implements*)) module))))

(defun implementation (slot)
  "Retrieves the implementing module."
  (module (gethash slot *radiance-implements*)))

(defsetf implementation implement)
