#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defgeneric module-name (identifier)
  (:documentation "The module name of an asdf module system."))
(defgeneric module-package (identifier)
  (:documentation "Retrieve the primary package of a module."))
(defgeneric module-identifier (identifier)
  (:documentation "Return the module identifier of a module."))
(defgeneric module-system (identifier)
  (:documentation "Return the ASDF system of a module."))

(defclass radiance-module (asdf:system)
  ((%implementation-map :initarg :implement :initform () :accessor implementation-map)
   (%module-instance :initarg :module-instance :initform NIL :accessor module-instance)
   (%module-package :initarg :module-package :initform *package* :accessor module-package))
  (:documentation "The base ASDF system class for radiance modules. Any module has to extend this system class."))

;; Just some sanity checks for the module definition.
(defmethod initialize-instance :after ((module radiance-module) &rest rest)
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

(defmacro define-module (system module-identifier-form &optional (package *package*))
  "Sets up the environment to recognize the specified module.
Specifically: Sets the :MODULE symbol-property on the PACKAGE-SYMBOL,
puts the MODULE-NAME onto the *radiance-modules* list, and maps the
PACKAGE to the SYSTEM in *radiance-package-map*."
  (with-gensyms ((package-gens "PACKAGE") (system-gens "SYSTEM"))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (let ((,package-gens ,package)
             (,system-gens ,system))
         (setf (get (package-symbol ,package-gens) :module) ,module-identifier-form)
         (pushnew (module-name ,system-gens) *radiance-modules*)
         (setf (gethash ,package-gens *radiance-package-map*) ,system-gens)))))

(defmethod asdf:operate :before ((op asdf:load-op) (module radiance-module) &key)
  (let* ((spec (module-instance module))
         (identifier (etypecase spec
                     (null (make-keyword (string-upcase (asdf:component-name module))))
                     (function (funcall spec))
                     (symbol spec))))
    (define-module module identifier (module-package module))))

(defmethod module-name ((module-name symbol))
  (module-name (module-system module-name)))

(defmethod module-name ((module radiance-module))
  (asdf:component-name module))

(defmethod module-name ((package package))
  (module-name (module-system package)))


(defmethod module-package ((module-name symbol))
  (module-package (module-system module-name)))

(defmethod module-package ((package package))
  package)


(defmethod module-identifier ((module-name symbol))
  (module-identifier (module-package module-name)))

(defmethod module-identifier ((module radiance-module))
  (module-identifier (module-package module)))

(defmethod module-identifier ((package package))
  (get (package-symbol package) :module))


(defmethod module-system ((module-name symbol))
  (asdf:find-system (string-downcase module-name)))

(defmethod module-system ((module radiance-module))
  module)

(defmethod module-system ((package package))
  (gethash package *radiance-package-map*))


(defmacro context-module (&optional (identifier *package*))
  "Retrieve the module-system active in the current code region.
Defaults to the module-system linked to the one active in the current *package*."
  `(module-system ,identifier))

(defmacro context-module-identifier (&optional (identifier *package*))
  "Retrieve the module-identifer active in the current code region.
Defaults to the module-identifier linked to the one active in the current *package*."
  (module-identifier identifier))


(defun compile-modules ()
  "Search through the ASDF systems and try to perform ASDF:LOAD-SYSTEM
on any that match the class RADIANCE-MODULE."
  (let ((module-class (find-class 'radiance-module)))
    (asdf:map-systems
     #'(lambda (sys)
         (when (eql (class-of sys) module-class)
           (asdf:load-system sys))))))
 
