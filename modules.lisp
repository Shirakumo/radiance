#|
This file is a part of Radiance
(c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-option-expander domain (package domain)
  `(setf (module-storage ,package :radiance-domain) ,(string-downcase domain)))

(defmethod domain ((module package))
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (or
     (module-storage module :radiance-domain)
     (string-downcase (module-name module)))))

(defmethod domain ((module symbol))
  (domain (module module)))

(define-resource-locator :domain (module)
  (domain module))

(define-option-expander permissions (package &rest perms)
  `(setf (module-storage ,package :radiance-permissions) ',perms))

(defun permissions (module)
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (module-storage module :radiance-permissions)))

(defun (setf permissions) (list module)
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (setf (module-storage module :radiance-permissions) list)))

(defun module-dependencies (module)
  (cond
    ;; ASDF 3.1+
    ((find-symbol "SYSTEM-DEPENDS-ON" "ASDF")
     (funcall (find-symbol "SYSTEM-DEPENDS-ON" "ASDF")
              (if (typep module 'asdf:system)
                  module
                  (virtual-module (module-name module)))))
    ;; ASDF 3.0 (and maybe 2, but who cares)
    ((find-symbol "COMPONENT-SIDEWAY-DEPENDENCIES" "ASDF")
     (funcall (find-symbol "COMPONENT-SIDEWAY-DEPENDENCIES" "ASDF")
              (if (typep module 'asdf:system)
                  module
                  (virtual-module (module-name module)))))
    (T (error "What the hell? Where's ASDF?"))))

(defun module-required-interfaces (module)
  (loop for dep in (module-dependencies module)
        when (and (listp dep) (eql (first dep) :interface))
        collect (second dep)))

(defun module-required-systems (module)
  (loop for dep in (module-dependencies module)
        unless (and (listp dep) (eql (first dep) :interface))
        collect dep))

(defun describe-module (module)
  (let ((module (module module))
        (virtual (virtual-module (module-name module))))
    (format T "Module ~a

Domain: ~a
Implements: ~:[Nothing~;~:*~{~a~^, ~}~]
Configuration: ~:[None~;~:*~{~a~^, ~}~]
Permissions: ~:[None~;~:*~{~a~^, ~}~]~%"
            (module-name module)
            (domain module)
            (implements module)
            (let ((table (config-tree (module-name module))))
              (when table (loop for name being the hash-keys of table collect name)))
            (permissions module))    
    (if virtual
        (format T "~%System: ~a
Required interfaces: ~:[None~;~:*~{~a~^, ~}~]
Required systems: ~:[None~;~:*~{~a~^, ~}~]
Author: ~:[None~;~:*~a~]~@[
Description: ~a~]"
                (asdf:component-name virtual)
                (module-required-interfaces virtual)
                (module-required-systems virtual)
                (asdf:system-author virtual)
                (asdf:system-description virtual))
        (format T "No corresponding system found!"))))

(defun create-module (name &key (base-file name) dependencies)
  (let* ((name (string-downcase name))
         (base-file (string-downcase base-file))
         (root (uiop:ensure-directory-pathname
                (merge-pathnames name (asdf:system-relative-pathname :radiance "modules/")))))
    ;; Create directories
    (ensure-directories-exist root)
    (ensure-directories-exist (merge-pathnames "template/" root))
    (ensure-directories-exist (merge-pathnames "static/" root))

    ;; Populate base ASD
    (with-open-file (s (merge-pathnames (format NIL "~a.asd" name) root) :direction :output)
      (format s "(in-package #:cl-user)~%~
 (asdf:defsystem #:~a
  :defsystem-depends-on (:radiance)
  :class \"radiance:module\"
  :components ((:file \"~a\"))
  :depends-on ~s)"
              name base-file dependencies))
    ;; Create base module file
    (with-open-file (s (merge-pathnames (format NIL "~a.lisp" base-file) root) :direction :output)
      (format s "(in-package #:rad-user)~%~
 (define-module #:~a
  (:use #:cl #:radiance))~%~
 (in-package #:~:*~a)~%~%" name))
    ;; Load system into quicklisp
    (when (find-package :ql)
      (funcall (symbol-function (find-symbol "REGISTER-LOCAL-PROJECTS" :ql)))
      (funcall (symbol-function (find-symbol "QUICKLOAD" :ql))
               (string-upcase name)))
    root))
