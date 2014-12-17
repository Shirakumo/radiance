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

(defmacro define-resource-locator (type (module &rest args) &body body)
  (assert (symbolp type) () "NAME must be a symbol.")
  (let ((type (intern (string-upcase type) *resource-locators*)))
    `(progn
       ,@(unless (fboundp type)
           `((defgeneric ,type ,(flatten-lambda-list (cons module args)))))
       (defmethod ,type (,module ,@args)
         ,@body))))

(defun resource (type module &rest args)
  (let ((module (module module)))
    (apply (or (find-symbol (string-upcase type) *resource-locators*)
               (error "Unknown resource type ~s" type))
           (if (interface-p module)
               (implementation module)
               module)
           args)))

(define-resource-locator domain (module)
  (domain module))

(define-resource-locator api (module page &rest args)
  (format NIL "/api/~a/~a?~{~a=~a~^&~}"
          (module-name module) page args))

(define-resource-locator static (module resource)
  (format NIL "/static/~a/~a"
          (module-name module) resource))

(define-resource-locator page (module name &rest args)
  (declare (ignore args))
  (path (or (uri-dispatcher (find-symbol (string-upcase name) module))
            (error "No page with name ~s found on ~s" name module))))
