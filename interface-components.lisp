#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-component-expander (defhook define-hook hook h) (interface name args &optional documentation)
  (let ((name (or (find-symbol (string name) interface)
                  (intern (string name) interface))))
    `(define-hook ,name ,args ,documentation)))

(define-component-expander (defresource define-resource define-resource-type resource) (interface type args &optional documentation)
  `(define-resource-type ,type ,args
     ,@(when documentation
         `((:documentation ,documentation)))))

(define-component-expander (defresourcelocator define-resource-locator locator) (interface type args)
  (let ((module (gensym "MODULE")))
    `(define-resource-locator ,type (,module (eql (load-time-value (interface ,(package-name interface))))) ,args
       (declare (ignore ,module ,@(lambda-fiddle:extract-lambda-vars args)))
       (error "Resource locator ~a not implemented for interface ~a!" ,(string-upcase type) ,(package-name interface)))))
