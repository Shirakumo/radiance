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
   (documentation :initform NIL :initarg :documentation :accessor item-documentation :type string))
  (:documentation "Radiance hook-item class"))

(defmethod print-object ((hook hook-item) out)
  (print-unreadable-object (hook out :type T)
    (format out "~a/~a/~a" (item-namespace hook) (name hook) (item-identifier hook)))
  hook)

(defun hook-equal (a b)
  "Checks if two hook-items designate the same (match in space, module and name)."
  (and (eql (item-identifier a) (item-identifier b))
       (hook-equalp a b)))

(defun hook-equalp (a b)
  "Checks if two hook-items designate the same (match in space and name)."
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
          (error 'namespace-conflict-error :namespace space :text (format nil "Namespace ~a already exists!" space)))
        (v:debug :radiance.server.hook "Creating empty namespace ~a" space)
        (setf (gethash space *radiance-hooks*) (make-hash-table))
        space)
    (change-name (name) 
      :report "Create a different namespace."
      :interactive read
      (define-namespace name))
    (redefine () 
      :report "Override the definition."
      (define-namespace space :ignore-defined T))
    (skip () 
      :report "Don't define anything.")))

(defun remove-namespace (space)
  "Remove a namespace altogether"
  (if (namespace space)
      (remhash space *radiance-hooks*)
      (error 'namespace-not-found-error :namespace space :text (format NIL "Attempted to remove namespace ~a, but it does not exist." space))))

(defun add-hook-item (space name identifier function &key documentation)
  (assert (symbolp name) () "Not a symbol: ~s" name)
  (assert (functionp function) () "Not a function: ~s" function)
  (assert (or (not documentation) (stringp documentation)) () "Not a string or NIL: ~s" documentation)
  (let ((namespace (gethash space *radiance-hooks*)))
    (if namespace
        (let* ((instance (make-instance 'hook-item :name name :space space :identifier identifier :function function :documentation documentation))
               (position (position instance (gethash name namespace) :test #'hook-equal)))
          (if position
              (setf (nth position (gethash name namespace)) instance)
              (push instance (gethash name namespace))))
        (error 'namespace-not-found-error :namespace space :text "Tried to add a hook on an undefined namespace."))))

(defun namespace (space &key ignore-undefined)
  "Retrieve a certain namespace."
  (let ((namespace (gethash space *radiance-hooks*)))
    (if (and (not ignore-undefined) (not namespace))
        (error 'namespace-not-found-error :namespace space :text "Tried to retrieve inexistent namespace."))
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

(defmacro define-hook ((space name) (&key (identifier `(context-module-identifier)) documentation) &body body)
  (with-gensyms ((identifiergens "IDENTIFIER"))
    `(let ((,identifiergens ,identifier))
       (add-hook-item ,space ,name ,identifiergens
                      #'(lambda () ,@body) :documentation ,documentation))))

(defun remove-hook (space hook)
  "Remove a hook from a namespace."
  (remhash hook (namespace space)))

(defun clear-hook-items (space hook)
  "Remove all items from a hook."
  (setf (gethash hook (namespace space)) NIL))

(unless (namespace :server :ignore-undefined T) (define-namespace :server))
(unless (namespace :page :ignore-undefined T) (define-namespace :page))
(unless (namespace :api :ignore-undefined T) (define-namespace :api))
(unless (namespace :user :ignore-undefined T) (define-namespace :user))
(unless (namespace :interface :ignore-undefined T) (define-namespace :interface))
