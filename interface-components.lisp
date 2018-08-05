#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-component-expander define-hook (interface name args &optional documentation)
  (let ((name (or (find-symbol (string name) interface)
                  (intern (string name) interface))))
    `(define-hook ,name ,args ,documentation)))

(define-component-expander define-hook-switch (interface on off args)
  (let ((on (intern (string on) interface))
        (off (intern (string off) interface)))
    `(progn ;; modularize-interfaces only exports the first symbol.
       (export ',off ,(package-name interface))
       (define-hook-switch ,on ,off ,args))))

(define-component-expander define-resource-locator (interface type args)
  `(unless (modularize-interfaces:implementation ,interface)
     (define-resource-locator ,(package-name interface) ,type ,args
       (declare (ignore ,@(lambda-fiddle:extract-lambda-vars args)))
       (error "Resource locator ~a not implemented for interface ~a!"
              ,(string-upcase type) ,(package-name interface)))))

(define-component-expander define-option-type (interface name)
  `',(or (find-symbol (string name) interface)
         (intern (string name) interface)))
