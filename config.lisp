#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *environment* "default")

(defun environment ()
  *environment*)

(defun (setf environment) (environment)
  ;; Clear config
  (dolist (module modularize:list-modules)
    (setf (module-storage module :config) NIL))
  ;; Update environment
  (setf *environment* environment)
  ;; Reload configuration
  (handler-case
      (ubiquitous:restore #.*package*)
    (ubiquitous:no-storage-file ()
      (warn "Configuration for ~s not found-- creating from defaults." environment)
      (ubiquitous:restore (asdf:system-relative-pathname :radiance-core "default-config.lisp"))
      (ubiquitous:offload #.*package*))))

(defun mconfig-pathname (module &optional (type :lisp))
  (ubiquitous:designator-pathname
   (if (and (module-p designator) (module-storage designator :config-file))
       (module-storage designator :config-file)
       (make-pathname :name (if (module-p designator)
                                (module-name designator)
                                (package-name designator))
                      :directory `(:relative "radiance" ,*environment*)))
   type))

(defmethod ubiquitous:designator-pathname ((designator package) type)
  (mconfig-pathname designator type))

(defun mconfig-storage (module)
  (let ((module (module module)))
    (or (module-storage module :config)
        (setf (module-storage module :config)
              (ubiquitous:with-local-storage (module :transaction NIL)
                (ubiquitous:restore))))))

(defun mconfig (module &rest path)
  (let ((module (module module)))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'ubiquitous:value path))))

(defun (setf mconfig) (value module &rest path)
  (let ((module (module module)))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'(setf ubiquitous:value) value path))))

(defun defaulted-mconfig (default module &rest path)
  (let ((module (module module)))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'ubiquitous:defaulted-value default path))))

(defmacro config (&rest path)
  `(mconfig ,*package* ,@path))

(defmacro defaulted-config (default &rest path)
  `(defaulted-mconfig ,default ,*package* ,@path))
