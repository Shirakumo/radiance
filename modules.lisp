#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-option-expander domain (package domain)
  (setf (module-storage package :radiance-domain) (string-downcase domain)))

(defmethod domain ((module package))
  (let ((module (if (interface-p module)
                    (or (implementation module)
                        (error "Interface ~s has no implementation; cannot retrieve domain." module))
                    (module module))))
    (or
     (module-storage module :radiance-domain)
     (string-downcase (module-name module)))))

(defmethod domain ((module symbol))
  (domain (module module)))

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

(defun describe-module-package (name stream)
  (when (module-p name)
    (let ((*print-pretty* T)
          (module (module name)))
      (pprint-logical-block (stream NIL)
        (format stream "~@:_~a names the radiance module ~a:" name module)
        (pprint-indent :block 2 stream)
        (format stream "~@:_Domain: ~s" (domain module))
        (format stream "~@:_Implements: ~<~;~:[Nothing~;~:*~{~s~^, ~:_~}~]~;~:>"
                (list (implements module)))
        (format stream "~@:_Claimed Pages: ~<~;~:[None~;~:*~{~a~^, ~:_~}~]~;~:>"
                (list (mapcar #'uri-dispatcher (module-storage module 'radiance-pages))))
        (format stream "~@:_API Endpoints: ~<~;~:[None~;~:*~{~(~a~)~^, ~:_~}~]~;~:>"
                (list (module-storage module 'radiance-apis)))
        (format stream "~@:_Configuration: ~<~;~:[None~;~:*~{~s~^, ~:_~}~]~;~:>"
                (list (let ((table (config-tree (module-name module))))
                        (when table (loop for name being the hash-keys of table collect name)))))
        (format stream "~@:_Permissions: ~<~;~:[None~;~:*~{~a~^, ~:_~}~]~;~:>"
                (list (permissions module)))
        (terpri stream)))))

(defun describe-module-system (name stream)
  (when (module-p name)
    (let ((*print-pretty* T)
          (virtual (virtual-module (module-name name))))
      (when virtual
        (pprint-logical-block (stream NIL)
          (format stream "~@:_~a names the ASDF system ~a:" name virtual)
          (pprint-indent :block 2 stream)
          (format stream "~@:_Version: ~:[Unknown~;~:*~a~]"
                  (asdf:component-version virtual))
          (format stream "~@:_Author: ~:[Unknown~;~:*~a~]"
                  (asdf:system-author virtual))
          (format stream "~@:_Homepage: ~:[Unknown~;~:*~a~]"
                  (asdf:system-homepage virtual))
          (format stream "~@:_License: ~:[Unknown~;~:*~a~]"
                  (asdf:system-license virtual))
          (format stream "~@:_Location: ~:[Unknown~;~:*~a~]"
                  (asdf:system-relative-pathname virtual ""))
          (format stream "~@:_Description: ~:[None~;~:*~a~]"
                  (asdf:system-description virtual))
          (format stream "~@:_Dependant Interfaces: ~<~;~:[None~;~:*~{~(~a~)~^, ~:_~}~]~;~:>"
                  (list (module-required-interfaces virtual)))
          (format stream "~@:_Dependant Systems: ~<~;~:[None~;~:*~{~a~^, ~:_~}~]~;~:>"
                  (list (module-required-systems virtual)))
          (terpri stream))))))

(defun describe-module (thing &optional (stream *standard-output*))
  (let ((*print-pretty* T))
    (format stream "~&~a~%  [~a]~%" thing (type-of thing))
    (describe-module-package thing T)
    (describe-module-system thing stream)))

(defun find-modules-directory ()
  (if (in-quicklisp-p :radiance)
      (merge-pathnames "radiance/modules/" (or (first ql:*local-project-directories*)
                                               (error "No local projects directory configured in Quicklisp; Don't know how to find the modules directory.")))
      (merge-pathnames "modules/" *root*)))

(defvar *modules-directory* (find-modules-directory))

(defun create-module (name &key (base-file name) dependencies (root *modules-directory*))
  (let* ((name (string-downcase name))
         (base-file (string-downcase base-file)))
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
