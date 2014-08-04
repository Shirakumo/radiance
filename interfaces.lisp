#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(define-condition interface-implementation-not-set (error)
  ((%requested :initarg :requested :initform (error "REQUESTED required.") :reader requested))
  (:report (lambda (c s) (format s "Interface ~s requested but no implementation is configured." (requested c)))))

(defmethod asdf::resolve-dependency-combination ((module module) (combinator (eql :interface)) args)
  (find-implementation (first args)))

(defun find-implementation (interface)
  (let* ((interface (interface interface))
         (configured-implementation (config-tree :interfaces (make-keyword (module-name interface)))))
    (unless configured-implementation
      (error 'interface-implementation-not-set :requested interface))
    (asdf:find-system configured-implementation T)))

(defun load-implementation (interface)
  (asdf:load-system (find-implementation interface)))
