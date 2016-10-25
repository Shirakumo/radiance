#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defvar *resource-locators* (or (find-package '#:radiance.resource-locators)
                                  (make-package '#:radiance.resource-locators
                                                :use () :nicknames '(#:org.shirakumo.radiance.core.resource.locators)))))

(defun resource-type (type)
  (or (find-symbol (string-upcase type) *resource-locators*)
      (error "Unknown resource type ~s" type)))

(define-condition implicit-resource-type-warning (style-warning)
  ((name :initarg :name :initform (error "NAME required.") :accessor name))
  (:report (lambda (c s) (format s "Implicitly creating new resource type ~a" (name c)))))

(defmacro define-resource-type (type args &body options)
  (assert (symbolp type) () "NAME must be a symbol.")
  ;; Convenience syntax for LOCATOR option.
  (let ((options (loop for option in options
                       when (eql (first option) :locator)
                       collect `(:method ,(list* (second option) (third option)) ,@(cdddr option)))))
    (let ((type (intern (string-upcase type) *resource-locators*)))
      `(defgeneric ,type (,(gensym "MODULE") ,@args)
         ,@options))))

(trivial-indent:define-indentation define-resource-type (4 4 &rest (&whole 2 0 4 &body)))

(defun remove-resource-type (type)
  (let ((type (resource-type type)))
    (fmakunbound type)
    (unintern type *resource-locators*)))

(defmacro define-resource-locator (type module args &body body)
  (assert (symbolp type) () "NAME must be a symbol.")
  (let ((type (intern (string-upcase type) *resource-locators*)))
    `(progn
       ,@(unless (fboundp type)
           (warn 'implicit-resource-type-warning :name type)
           `((define-resource-type ,type ,(flatten-method-lambda-list args))))
       (defmethod ,type (,module ,@args)
         ,@body))))

(declaim (inline resource))
(defun resource (type module &rest args)
  (apply (resource-type type) (module module) args))

(define-resource-type domain ()
  (:locator module ()
    (make-uri :domains (list (domain module)))))

(define-resource-type api (page &rest args)
  (:locator module (page &rest args)
    (make-uri :path (format NIL "/api/~a/~a?~{~a=~a~^&~}"
                            (module-name module) page args))))

(define-resource-type static (resource)
  (:locator module (resource)
    (make-uri :path (format NIL "/static/~a/~a"
                            (module-name module) resource))))

(define-resource-type page (name &rest args)
  (:locator module (name &rest args)
    (declare (ignore args))
    (or (uri-dispatcher (find-symbol (string-upcase name) module))
        (error "No page with name ~s found on ~s" name module))))
