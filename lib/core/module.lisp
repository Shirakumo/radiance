#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass interface (asdf:system)
  ((%interface-name :initarg :interface-name :initform (error "INTERFACE-NAME required.") :accessor interface-name)))

(defclass module (asdf:system)
  ((%implement :initarg :implement :initform () :accessor implement)))

(defmethod asdf:operate ((op asdf:load-op) (interface interface) &key)
  (let* ((name (interface-name interface))
         (implementation (config-tree :implementation name)))
    (if implementation
        (let ((system (asdf:find-system implementation)))
          (asdf:load-system system)
          (let ((package (find-package name)))
            (if package
                (setf (symbol-value (find-symbol "*IMPLEMENTATION*" package))
                      (make-instance (cdr (assoc name (implement system)))))
                (error 'no-such-interface-error :interface name))))
        (error 'no-interface-implementation-error :interface name))))

