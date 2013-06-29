#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-modules* (make-hash-table) "Map of all loaded modules.")

(define-condition module-already-initialized (error)
  ((module :initarg :module :reader module)))

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

(defclass module ()
  ((name :initform NIL :initarg :name :reader name :type string :allocation :class)
   (author :initform NIL :initarg :author :reader author :type string :allocation :class)
   (version :initform NIL :initarg :version :reader version :type string :allocation :class)
   (license :initform NIL :initarg :license :reader license :type string :allocation :class)
   (url :initform NIL :initarg :url :reader url :type string :allocation :class)
   
   (dependencies :initform () :initarg :dependencies :reader dependencies :type list :allocation :class)
   (collections :initform () :initarg :collections :reader collections :type list :allocation :class)
   (callables :initform () :initarg :callables :reader callables :type list :allocation :class)
   (persistent :initform T :initarg :persistent :reader persistent :type boolean :allocation :class))
  (:documentation "Radiance base module class."))

(defmethod print-object ((mod module) out)
  (print-unreadable-object (mod out :type t)
    (if (version mod) (format out "v~a" (version mod)))))

(defgeneric init (module)
  (:documentation "Called when Radiance is started up."))

(defgeneric shutdown (module)
  (:documentation "Called when Radiance is shut down."))

(defmacro defmodule (name superclasses docstring metadata &rest extra-slots)
  "Define a new Radiance module."
  (let* ((superclasses (if (not superclasses) '(module) superclasses))
         (classdef `(defclass ,name ,superclasses
                      (,@extra-slots)
                      (:documentation ,docstring)))
         (initializer `(setf (gethash ',name *radiance-modules*)
                             (make-instance ',name ,@metadata))))
    `(restart-case (if (gethash ',name *radiance-modules*)
                       (error 'module-already-initialized :module ',name)
                       (progn ,classdef ,initializer))
       (override-both () 
         :report "Redefine the module and create a new instance of the module anyway."
         ,classdef ,initializer)
       (override-module ()
         :report "Just redefine the module."
         ,classdef)
       (override-instance ()
         :report "Just create a new instance of the module."
         ,initializer)
       (do-nothing ()
         :report "Leave module and instance as they are."))))

(defun make-column (name &key (access-mode "000") description)
  "Shorthand function to create a new column instance."
  (make-instance 'column :name name :access-mode access-mode :description description))

(defmacro make-collection (name (&key (access-mode "000") description) &rest columns)
  "Create a new representation of a collection."
  `(make-instance 'collection :name ,(symbol-name name) :access-mode ,access-mode :description ,description
                  :columns ,(loop with array = (make-array (length columns) :element-type 'column :fill-pointer 0)
                                 for column in columns
                                 do (vector-push (make-column column) array)
                                 finally (return array))))

(defun get-module (module)
  "Retrieves the requested module from the instance list."
  (gethash module *radiance-modules*))
