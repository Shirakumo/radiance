#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmacro make-module-class (name superclasses docstring &optional extra-slots)
  `(defclass ,name ,superclasses
     ((name :initarg :name :reader name :type string :allocation :class)
      (author :initarg :author :reader author :type string :allocation :class)
      (version :initarg :version :reader version :type string :allocation :class)
      (license :initarg :license :reader license :type string :allocation :class)
      (url :initarg :url :reader url :type string :allocation :class)
      
      (collections :initarg :collections :reader collections :type list :allocation :class)
      (persistent :initform T :initarg :persistent :reader persistent :type boolean :allocation :class)
                            
      (implements :initarg :implements :reader implementations :type list :allocation :class)
      (asdf-system :initarg :asdf-system :reader asdf-system :type symbol :allocation :class)
      (dependencies :initarg :dependencies :reader dependencies :type list :allocation :class)
      (compiled :initarg :compiled :accessor compiled-p :type boolean :allocation :class)
      ,@extra-slots)
     (:documentation ,docstring)))

(make-module-class module () "Radiance base module class")

(defmacro defmodule (name superclasses docstring 
                     (&key fullname author version license url collections (persistent T) implements asdf-system dependencies compiled)
                     (&body defsystem)
                     &body extra-slots)
  "Define a new Radiance module."
  (let ((superclasses (if (not superclasses) '(module) superclasses))
        (classdef (gensym "CLASSDEF"))
        (initializer (gensym "INITIALIZER"))
        (asdf-system (or asdf-system (format nil "RADIANCE-MOD-~a" name))))
    `(eval-when (:compile-toplevel :load-toplevel :execute) 
       
       ,(if defsystem
            `(asdf:defsystem ,(intern (string-upcase asdf-system))
               :name ,fullname
               :license ,license
               :author ,author
               :version ,version
               :description ,docstring
               ,@defsystem))

       (flet ((,classdef () (log:info "Defining module ~a" ',name) (make-module-class ,name ,superclasses ,docstring ,extra-slots))
              (,initializer () (log:info "Initializing module ~a" ',name)
                            (let ((instance (make-instance ',name 
                                                       :name ,fullname :author ,author :version ,version :license ,license :url ,url
                                                       :collections ,collections :persistent ,persistent
                                                       :implements ,implements :asdf-system ,asdf-system :dependencies ,dependencies
                                                       :compiled ,compiled)))
                              (restart-case
                                  (progn 
                                    (unless (eql (class-of instance) (class-of (get (package-symbol *package*) :module)))
                                      (error 'module-already-initialized :module (get (package-symbol *package*) :module)))
                                    (setf (get (package-symbol *package*) :module) instance)
                                    (setf (gethash (make-keyword ',name) *radiance-modules*) instance))
                                (override-existing-module ()
                                  :report "Override the existing module in the package (could break things!)"
                                  (setf (get (package-symbol *package*) :module) instance)
                                  (setf (gethash (make-keyword ',name) *radiance-modules*) instance))))))
         
         (restart-case (if (gethash ',name *radiance-modules*)
                           (error 'module-already-initialized :module ',name)
                           (progn (,classdef) (,initializer)))
           (override-both () 
             :report "Redefine the module and create a new instance of the module anyway."
             (,classdef) (,initializer))
           (override-module ()
             :report "Just redefine the module."
             (,classdef))
           (override-instance ()
             :report "Just create a new instance of the module."
             (,initializer))
           (do-nothing ()
             :report "Leave module and instance as they are."))))))

(defmethod print-object ((mod module) out)
  (print-unreadable-object (mod out :type t)
    (if (version mod) (format out "v~a" (version mod)))))

(defgeneric init (module)
  (:documentation "Called when Radiance is started up."))

(defgeneric shutdown (module)
  (:documentation "Called when Radiance is shut down."))

(defgeneric get-module (module)
  (:documentation "Retrieves the requested module from the instance list."))

(defmethod get-module ((module module)) module)

(defmethod get-module ((module symbol))
  "Retrieves the requested module from the instance list."
  (get-module (symbol-name module)))

(defmethod get-module ((module string))
  "Retrieves the requested module from the instance list."
  (gethash (make-keyword module) *radiance-modules*))

(defmethod get-module ((module (eql T)))
  "Retrieves the module of the current package, if any."
  (get (package-symbol *package*) :module))

(defun module-package (module)
  "Retrieve the package of the module."
  (symbol-package (class-name (class-of (get-module module)))))

(defun discover-modules (&key redefine reinitialize)
  (cl-fad:walk-directory (merge-pathnames "mod/" (pathname (config :root)))
                         (lambda (file) (load-module file :redefine redefine :reinitialize reinitialize))
                         :test (lambda (file) 
                                 (let ((name (file-namestring file)))
                                   (equal ".mod" (subseq name (- (length name) 4)))))))

(defun load-module (file &key redefine reinitialize &allow-other-keys)
  (log:info "Discovered mod file: ~a (~a)" file (file-namestring file))
  (handler-bind ((module-already-initialized
                  #'(lambda (c) (declare (ignore c)) 
                            (cond
                              ((not (or redefine reinitialize)) (invoke-restart 'do-nothing))
                              ((and redefine reinitialize) (invoke-restart 'override-both))
                              (redefine (invoke-restart 'override-module))
                              (reinitialize (invoke-restart 'override-instance))))))
    (load file)))

(defgeneric compile-module (module &key force)
  (:documentation "Compiles a module's source files."))

(defmethod compile-module ((module module) &key force)
  (when (or (not (slot-value module 'compiled)) force)
    (log:info "Compiling module ~a (~a)" module (asdf-system module))
    (loop for dependency in (slot-value module 'dependencies)
       do (compile-dependency dependency :force force))
    (ql:quickload (asdf-system module))
    (setf (slot-value module 'compiled) T)))

(defmethod compile-module ((module string) &key force)
  (let ((module-instance (get-module module)))
    (if module-instance
        (compile-module module-instance :force force)
        (error "Module ~a not found." module))))

(defmethod compile-module ((module symbol) &key force)
  (compile-module (symbol-name module) :force force))

(defun compile-dependency (dependency &key force)
  (cond
    ((get-module dependency) (compile-module dependency :force force))
    ((config-tree :implementations (make-keyword dependency)) 
     (let ((deps (config-tree :implementations (make-keyword dependency))))
       (if (listp deps)
           (loop for dep in deps do (compile-module dep :force force))
           (compile-module deps :force force))))
    (T (error "Dependency ~a not found!" dependency))))



(defclass column ()
  ((name :initform (error "Column name required.") :initarg :name :reader name :type string)
   (access-mode :initform "000" :initarg :access-mode :reader access-mode :type string)
   (description :initform NIL :initarg :description :reader description :type string))
  (:documentation "Abstract database column class for metadata purposes."))

(defclass collection (column)
  ((columns :initform (error "List of columns required.") :initarg :columns :reader columns :type simple-vector))
  (:documentation "Abstract database collection class for metadata purposes."))

(defmethod print-object ((col column) out)
  (print-unreadable-object (col out :type t)
    (format out "~a (~a)" (name col) (access-mode col))))

(defun make-column (name &key (access-mode "000") description)
  "Shorthand function to create a new column instance."
  (make-instance 'column :name name :access-mode access-mode :description description))

(defun make-collection (name &key (access-mode "000") description columns)
  "Create a new representation of a collection."
  (make-instance 'collection :name name :access-mode access-mode :description description
                 :columns (loop with array = (make-array (length columns) :element-type 'column :fill-pointer 0)
                             for column in columns
                             do (vector-push (if (listp column) 
                                                 (destructuring-bind (name &optional mode description) column
                                                   (make-column name :access-mode mode :description description))
                                                 (make-column column)) array)
                             finally (return array))))

