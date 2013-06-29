#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-implements* (make-hash-table) "Radiance implements table.")

(defclass implementation ()
  ((module :initform NIL :initarg :module :accessor module :type module)
   (superclass :initform (error "Superclass required.") :initarg :superclass :accessor superclass :type symbol))
  (:documentation "Radiance implementation class to hold information about an implementations slot."))

(defmethod print-object ((impl implementation) out)
  (print-unreadable-object (impl out :type t)
    (format out "~a -> ~a" (superclass impl) (module impl))))

(defgeneric implement (slot module)
  (:documentation "Registers a module for an implementation."))

(defmethod implement ((slot symbol) (module module))
  "Standard implements function for non-existent symbols."
  (error "Implementation ~a unknown!" slot))

(defmacro defimplclass (slot superclass)
  "Defines an implementations interface class."
  `(progn 
     (setf (gethash ',slot *radiance-implements*)
           (make-instance 'implementation :superclass ',superclass))
     (defmethod implement ((slot (eql ',slot)) (module module))
       "Standard implements function for badly requested classes."
       (error "Module does not match implementation superclass ~a!" slot))
     (defmethod implement ((slot (eql ',slot)) (module ,superclass))
       (setf (module (gethash ',slot *radiance-implements*)) module))
     ',superclass))

(defmacro defimpl (slot &rest generics)
  "Define a new implementation."
  (let ((documentation "")
        (mod-gens (gensym "IMPL-GENSYM")))
    (when (stringp (car generics))
      (setf documentation (car generics)
            generics (cdr generics)))
    `(progn
       (defclass ,slot (module) ()
         (:documentation ,documentation))
       ,@(loop for generic in generics collect 
              (let ((func (first generic))
                    (args (append (second generic)
                                  (if (not (find '&key (second generic))) '(&key)) 
                                  '(&allow-other-keys)))
                    (doc (third generic)))
                `(progn
                   (defgeneric ,func ,(append (list mod-gens) args)
                     (:documentation ,doc))
                   (defmethod ,func ,(append `((,mod-gens ,slot)) args)
                     ,(format nil "Standard method implementation for ~a's ~a, always throws an error." slot func)
                     
                     (declare (sb-ext:muffle-conditions style-warning))
                     (error "Module ~a does not implement required method ~a!" ,mod-gens ',func)))))
       (defimplclass ,slot ,slot))))

(defun implementation (slot)
  "Retrieves the implementing module."
  (module (gethash slot *radiance-implements*)))

(defsetf implementation implement)
