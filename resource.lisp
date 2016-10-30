#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *resource-locators* (make-hash-table :test 'equal))

(defun resource-type (type)
  (or (gethash (string-downcase type) *resource-locators*)
      (error "Unknown resource type ~s" type)))

(defun (setf resource-type) (table type)
  (setf (gethash (string-downcase type) *resource-locators*) table))

(defun remove-resource-type (type)
  (remhash (string-downcase type) *resource-locators*))

(defun resource-locator (type ident)
  (or (gethash (string-downcase ident) (resource-type type))
      (if (eql ident T)
          (error "No default resource locator for resource type ~s." type)
          (resource-locator type T))))

(defun (setf resource-locator) (value type ident)
  (setf (gethash (string-downcase ident) (resource-type type))
        value))

(defmacro define-resource-type (type args &body default)
  (let ((type (string-downcase type)))
    `(progn (setf (resource-type ,type)
                  (gethash ,type *resource-locators*
                           (make-hash-table :test 'equal)))
            ,@(when default
                `((setf (gethash T (resource-type ,type))
                         (lambda ,args ,@default)))))))

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
               (destructuring-bind ,args ,argsg
                 ,@body))))))

(defun resource (module type &rest args)
  (apply (resource-locator type module) (module module) args))

(define-compiler-macro resource (&whole whole &environment env module type &rest args)
  (if (and (constantp module env) (constantp type env))
      `(apply (load-time-value (resource-locator ,type ,module))
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
