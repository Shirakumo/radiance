#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *resource-types* (make-hash-table :test 'equal))

(define-documentable resource-type ()
  ((name :initarg :name :accessor name)
   (locators :initarg :locators :accessor locators))
  (:default-initargs
   :name (error "NAME required.")
   :locators (make-hash-table :test 'equal))
  (:find-function resource-type))

(defmethod print-object ((type resource-type) stream)
  (print-unreadable-object (type stream :type T)
    (format stream "~a" (name type))))

(defun resource-type (name)
  (or (gethash (string-downcase name) *resource-types*)
      (error "Unknown resource type ~s" name)))

(defun (setf resource-type) (type name)
  (setf (gethash (string-downcase name) *resource-types*) type))

(defun remove-resource-type (type)
  (remhash (string-downcase type) *resource-types*))

(defun list-resource-types ()
  (loop for name being the hash-keys of *resource-types* collect name))

(defun resource-locator (type ident)
  (let ((locators (locators (resource-type type))))
    (if (eql ident T)
        (or (gethash T locators)
            (error "No default resource locator for resource type ~s." type))
        (gethash (string-downcase ident) locators))))

(defun (setf resource-locator) (value type ident)
  (setf (gethash (string-downcase ident) (locators (resource-type type)))
        value))

(defmacro define-resource-type (type args &body default)
  (let ((type (string-downcase type)))
    `(progn (setf (resource-type ,type)
                  (gethash ,type *resource-types*
                           (make-instance 'resource-type :name ,type)))
            (setf (gethash T (locators (resource-type ,type)))
                  ,(if default
                       `(lambda ,args ,@default)
                       NIL)))))

(defmacro define-resource-locator (module type args &body body)
  (assert (symbolp type) () "NAME must be a symbol.")
  (let ((type (string-downcase type))
        (module (string-downcase module))
        (moduleg (gensym "MODULE"))
        (argsg (gensym "ARGS")))
    `(setf (resource-locator ,type ,module)
           (lambda (,moduleg &rest ,argsg)
             (flet ((call-default-locator ()
                      (apply (resource-locator ,type T) ,moduleg ,argsg)))
               (declare (ignorable #'call-default-locator))
               (destructuring-bind ,args ,argsg
                 ,@body))))))

(defun resource (module type &rest args)
  (apply (resource-locator type module) (module module) args))

(define-compiler-macro resource (&whole whole &environment env module type &rest args)
  (if (and (constantp module env) (constantp type env))
      `(funcall (load-time-value (resource-locator ,type ,module))
                (load-time-value (module ,module))
                ,@args)
      whole))

(define-resource-type domain (module)
  (make-uri :domains (list (domain module))))

(define-resource-type api (module page &rest args)
  (make-uri :path (format NIL "/api/~a/~a?~{~a=~a~^&~}"
                          (module-name module) page args)))

(define-resource-type static (module resource)
  (make-uri :path (format NIL "/static/~a/~a"
                          (module-name module) resource)))

(define-resource-type page (module name &rest args)
  (declare (ignore args))
  (or (uri-dispatcher (find-symbol (string-upcase name) module))
      (error "No page with name ~s found on ~s" name module)))
