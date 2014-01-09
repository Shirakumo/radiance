#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass module (asdf:system)
  ((%implement :initarg :implement :initform () :accessor implement)
   (%module-instance :initarg :module-instance :initform NIL :accessor module-instance)
   (%module-package :initarg :module-package :initform *package* :accessor module-package)))

;; Just some sanity checks for the module definition.
(defmethod initialize-instance :after ((module module) &rest rest)
  (declare (ignore rest))
  (assert (listp (implement module)) () "A module's implement map has to be a list of keyword -> identifier pairs.")
  (mapc #'(lambda (definition)
            (let ((interface (car definition))
                  (identifier (if (consp (cdr definition)) (second definition) (cdr definition))))
              (assert (keywordp interface) () "Each interface name in the implement map has to be a keyword.")
              (etypecase identifier (function) (symbol) (standard-object))))
        (implement module))
  (let ((spec (module-instance module)))
    (etypecase spec
      (function)
      (symbol)
      (list (assert (null spec) () "A module's module-instance slot cannot be a list.")))))

(defmethod asdf:operate :before ((op asdf:load-op) (module module) &key)
  (let* ((spec (module-instance module))
         (instance (etypecase spec
                     (list
                      (assert (null spec) () "MODULE-INSTANCE has to be one of NIL, FUNCTION, SYMBOL.")
                      (asdf:component-name module))
                     (function (funcall spec))
                     (symbol spec))))
    (define-module module instance (module-package module))))

(defmacro define-module (system module-instance-form &optional (package *package*))
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (get (package-symbol ,package) :module) ,module-instance-form)
     (pushnew (module-name ,system) *radiance-modules*)))


(defgeneric module-name (identifier)
  (:documentation "The module name of an asdf module system."))

(defmethod module-name ((module module))
  (asdf:component-name module))


(defun package-module-identifier (&optional (package *package*))
  "Retrieve the module-identifier stored in the package."
  (get (package-symbol package) :module))

(defgeneric module-package (identifier)
  (:documentation "Retrieve the primary package of a module."))

(defmethod module-package ((module-name symbol))
  (module-package (module-system module-name)))


(defgeneric module-identifier (identifier)
  (:documentation "Return the module identifier used in interfaces and the like for a given module."))

(defmethod module-identifier ((module-name symbol))
  (package-module-identifier (module-package module-name)))

(defmethod module-identifier ((module module))
  (package-module-identifier (module-package module)))


(defgeneric module-system (identifier)
  (:documentation "Return the ASDF system of a module."))

(defmethod module-system ((module-name symbol))
  (asdf:find-system module-name))

(defmethod module-system ((module module))
  module)
