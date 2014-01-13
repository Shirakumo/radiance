#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass hook-item ()
  ((name :initform (error "Hook name required.") :initarg :name :accessor name :type symbol)
   (space :initform (error "Namespace required.") :initarg :space :accessor item-namespace :type symbol)
   (identifier :initform (error "Hook identifier required.") :initarg :identifier :accessor item-identifier)
   (function :initform (error "Hook module method required.") :initarg :function :accessor item-function :type function)
   (description :initform NIL :initarg :description :accessor item-description :type string))
  (:documentation "Radiance hook-item class"))

(defmethod print-object ((hook hook) out)
  (print-unreadable-object (hook out :type T)
    (format out "~a/~a/~a" (item-namespace hook) (name hook) (item-identifier hook))))

(defmethod hook-equal ((a hook) (b hook))
  "Checks if two hooks designate the same (match in space, module and name)."
  (and (eql (item-identifier a) (item-identifier b))
       (hook-equalp a b)))

(defmethod hook-equalp ((a hook) (b hook))
  "Checks if two hooks designate the same (match in space and name)."
  (and (eql (name a) (name b))
       (eq (item-namespace a) (item-namespace b))))

(defun namespace-map ()
  "Retrieve the hash-map that contains all trigger namespaces."
  *radiance-hooks*)

(defun define-namespace (space &key ignore-defined)
  "Create a certain namespace."
  (assert (symbolp space) () "Not a symbol: ~s" space)
  (restart-case 
      (let ((namespace (gethash space *radiance-hooks*)))
        (when (and (not ignore-defined) namespace)
          (error 'namespace-conflict :text (format nil "Namespace ~a already exists!" space)))
        (v:debug :radiance.server.hook "Creating empty namespace ~a" space)
        (setf (gethash space *radiance-hooks*) (make-hash-table))
        space)
    (change-name (name) 
      :report "Create a different namespace."
      :interactive read
      (add-namespace name))
    (redefine () 
      :report "Override the definition."
      (add-namespace space :ignore-defined T))
    (skip () 
      :report "Don't define anything.")))

(defun add-hook-item (space name identifier function &key description)
  (assert (symbolp name) () "Not a symbol: ~s" name)
  (assert (functionp function) () "Not a function: ~s" function)
  (assert (or (not description) (stringp description)) () "Not a string or NIL: ~s" description)
  (let ((namespace (gethash space *radiance-hooks*)))
    (if namespace
        (let* ((instance (make-instance 'hook :name name :space space :identifier identifier :function function :description description))
               (position (position instance (gethash name namespace) :test #'hook-equal)))
          (if position
              (setf (nth position (gethash name namespace)) instance)
              (push instance (gethash name namespace))))
        (error 'namespace-not-found :namespace space :text "Tried to add a hook on an undefined namespace."))))

(defun namespace (space &key ignore-undefined)
  "Retrieve a certain namespace."
  (let ((namespace (gethash space *radiance-hooks*)))
    (if (and (not ignore-undefined) (not namespace))
        (error 'namespace-not-found :namespace space :text "Tried to retrieve inexistent namespace."))
    namespace))

(defun hooks (space)
  "Retrieve all hooks of a namespace."
  (alexandria:hash-table-keys (namespace space)))

(defun hook-items (space hook)
  "Retrieve all hook items for a hook."
  (gethash hook (namespace space)))

(defun trigger (space hook)
  "Trigger a certain hook and collect all return values."
  (v:trace :radiance.server.hook "Triggering hook ~a/~a" space hook)
  (loop for item in (hook-items space hook)
     if (funcall (item-function item))
     collect it))

(defmacro define-hook ((space name) (&key (identifier `(module-identifier (get-module))) description) &body body)
  (with-gensyms ((identifiergens "IDENTIFIER"))
    `(let ((,identifiergens ,identifier))
       (add-hook-item ,space ,name ,identifiergens
                      #'(lambda () ,@body) :description ,description))))

(add-namespace :server)
(add-namespace :api)
(add-namespace :page)
(add-namespace :user)
