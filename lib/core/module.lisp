#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass module (asdf:system)
  ((%implementation-map :initarg :implement :initform () :accessor implementation-map)
   (%module-instance :initarg :module-instance :initform NIL :accessor module-instance)
   (%module-package :initarg :module-package :initform *package* :accessor module-package)))

;; Just some sanity checks for the module definition.
(defmethod initialize-instance :after ((module module) &rest rest)
  (declare (ignore rest))
  (assert (listp (implementation-map module)) () "A module's implementation-map has to be a list of keyword -> identifier pairs.")
  (mapc #'(lambda (definition)
            (let ((interface (car definition))
                  (identifier (if (consp (cdr definition)) (second definition) (cdr definition))))
              (assert (keywordp interface) () "Each interface name in the implementation-map has to be a keyword.")
              (etypecase identifier (function) (symbol) (standard-object))))
        (implementation-map module))
  (let ((spec (module-instance module)))
    (etypecase spec (function) (symbol) (null))))

(defmethod asdf:operate :before ((op asdf:load-op) (module module) &key)
  (let* ((spec (module-instance module))
         (instance (etypecase spec
                     (null (asdf:component-name module))
                     (function (funcall spec))
                     (symbol spec))))
    (define-module module instance (module-package module))))

(defmacro define-module (system module-instance-form &optional (package *package*))
  (with-gensyms ((package-gens "PACKAGE") (system-gens "SYSTEM"))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,package-gens ,package)
             (,system-gens ,system))
         (setf (get (package-symbol ,package-gens) :module) ,module-instance-form)
         (pushnew (module-name ,system-gens) *radiance-modules*)
         (setf (gethash ,package-gens *radiance-package-map*) ,system-gens)))))


(defgeneric module-name (identifier)
  (:documentation "The module name of an asdf module system."))

(defmethod module-name ((module-name symbol))
  (module-name (module-system module-name)))

(defmethod module-name ((module module))
  (asdf:component-name module))

(defmethod module-name ((package package))
  (module-name (module-system package)))


(defgeneric module-package (identifier)
  (:documentation "Retrieve the primary package of a module."))

(defmethod module-package ((module-name symbol))
  (module-package (module-system module-name)))

(defmethod module-package ((package package))
  package)


(defgeneric module-identifier (identifier)
  (:documentation "Return the module identifier of a module."))

(defmethod module-identifier ((module-name symbol))
  (module-identifier (module-package module-name)))

(defmethod module-identifier ((module module))
  (module-identifier (module-package module)))

(defmethod module-identifier ((package package))
  (get (package-symbol package) :module))


(defgeneric module-system (identifier)
  (:documentation "Return the ASDF system of a module."))

(defmethod module-system ((module-name symbol))
  (asdf:find-system module-name))

(defmethod module-system ((module module))
  module)

(defmethod module-system ((package package))
  (gethash package *radiance-package-map*))


(defun get-module (&optional (identifier *package*))
  (module-system identifier))
