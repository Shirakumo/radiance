#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.core)

(defvar *config* NIL)
(defvar *root* (asdf:system-source-directory :radiance))
(defvar *config-path* (merge-pathnames (make-pathname :name "radiance.uc" :type "lisp") *root*))
(defvar *data-path* (merge-pathnames (make-pathname :directory '(:relative "data")) *root*))

(defun load-config (&optional (path *config-path*))
  (setf *config* (uc:load-configuration path :format :lisp)
        *config-path* path)
  T)

(defun config-tree (&rest branches)
  (let ((uc:*config* *config*))
    (apply #'uc:config-tree branches)))

(defun (setf config-tree) (value &rest branches)
  (let ((uc:*config* *config*))
    (apply #'(setf uc:config-tree) value branches)))
