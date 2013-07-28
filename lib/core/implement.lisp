#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

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
     (log:info "Defining implementation ~a with ~a" ',slot ',superclass)
     (setf (gethash ',slot *radiance-implements*)
           (make-instance 'implementation :superclass ',superclass))
     (defmethod implement ((slot (eql ',slot)) (module module))
       "Standard implements function for badly requested classes."
       (error "Module does not match implementation superclass ~a!" slot))
     (defmethod implement ((slot (eql ',slot)) (module ,superclass))
       (log:info "~a implements ~a" module slot)
       (setf (module (gethash ',slot *radiance-implements*)) module))
     ',superclass))

(defmacro defimpl (slot &rest generics)
  "Define a new implementation. A generics definition is a list of the following format: (function-name (additional-args*) docstring?)"
  (let ((documentation "") (mod-gens (gensym "IMPL-GENSYM"))
        (super (if (listp slot) (cdr slot) '(module)))
        (slot (if (listp slot) (first slot) slot)))
    (when (stringp (car generics))
      (setf documentation (car generics)
            generics (cdr generics)))
    (log:info "Generating implementation ~a with superclasses ~a." slot super)
    `(progn
       (defclass ,slot ,super ()
         (:documentation ,documentation))
       ,@(loop for generic in generics collect
              (destructuring-bind (func args &optional doc) generic
                (let* ((args (append args
                                    (if (not (find '&key args)) '(&key))
                                    '(&allow-other-keys)))
                      (gen-args (loop for arg in args collect (if (listp arg) (first arg) arg))))
                  `(progn
                     (defgeneric ,func ,(append (list mod-gens) gen-args)
                       (:documentation ,doc))
                     (defmethod ,func ,(append `((,mod-gens ,slot)) args)
                       ,(format nil "Standard method implementation for ~a's ~a, always throws an error." slot func)
                       
                       #+sbcl (declare (sb-ext:muffle-conditions style-warning))
                       (error "Module ~a does not implement required method ~a!" ,mod-gens ',func))
                     (defmethod ,func ,(append `((,mod-gens (eql T))) args)
                       ,(format nil "Standard method implementation for ~a's ~a, always redirects to current implementation." slot func)
                       (funcall #',func (implementation ',slot) ,@(args-to-funcall gen-args)))))))
       
       (defimplclass ,slot ,slot))))

(defun args-to-funcall (args)
  (alexandria:flatten
   (loop with kwargs = NIL 
      for var in args
      unless (if (or (eq var '&key) (eq var '&allow-other-keys)) (setf kwargs T))
      collect (if kwargs (list (make-keyword var) var) var))))

(defun implementation (slot)
  "Retrieves the implementing module."
  (module (gethash slot *radiance-implements*)))

(defsetf implementation implement)

(defun load-implementations (&key force)
  (loop for (key . val) in (config :implementations)
     do (progn (log:info "Choosing ~a for ~a." val key)
               (if (listp val)
                   (loop for module in val 
                      do (compile-module module :force force))
                   (compile-module val :force force)))))
