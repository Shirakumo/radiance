#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-option-expander domain (package domain)
  (setf (module-storage package :radiance-domain) (string-downcase domain)))

(defun module-domain (module)
  (let ((module (if (interface-p module)
                    (or (implementation module)
                        (error 'interface-implementation-not-present :interface module))
                    (module module))))
    (or
     (module-storage module :radiance-domain)
     (string-downcase (module-name module)))))

(defmethod domain ((module package))
  (module-domain module))

(defmethod domain ((module symbol))
  (domain (module module)))

(define-option-expander permissions (package &rest perms)
  `(setf (module-storage ,package :radiance-permissions) ',perms))

(defun module-permissions (module)
  (let ((module (if (interface-p module)
                    (implementation module)
                    (module module))))
    (module-storage module :radiance-permissions)))

(defun (setf module-permissions) (list module)
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

(defun module-pages (module)
  (mapcar #'uri-dispatcher (module-storage module 'radiance-pages)))

(defun module-api-endpoints (module)
  (module-storage module 'radiance-apis))

(defun describe-module (module stream)
  (let* ((*print-pretty* T)
         (*print-circle* NIL)
         (module module)
         (virtual (virtual-module (module-name module))))
    (when virtual
      (pprint-logical-block (stream NIL)
        (format stream "~@:_~a is associated with the virtual module ~a:" (module-name module) virtual)
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
        (terpri stream)))
    (cond ((interface-p module)
           (pprint-logical-block (stream NIL)
             (format stream "~@:_~a is a radiance interface:" (module-name module))
             (pprint-indent :block 2 stream)
             (format stream "~@:_Implemented by: ~:[Nothing~;~:*~a~]"
                     (implementation module))
             (format stream "~@:_Definition:")
             (pprint-indent :block 4 stream)
             (format stream "~@:_~<~;~:[None~;~:*~{~s~^ ~:_~}~]~;~:>"
                     (list (module-storage module 'modularize-interfaces::interface-definition)))
             (terpri stream)))
          (T
           (pprint-logical-block (stream NIL)
             (format stream "~@:_~a is a radiance module:" (module-name module))
             (pprint-indent :block 2 stream)
             (format stream "~@:_Domain: ~s" (module-domain module))
             (format stream "~@:_Implements: ~<~;~:[Nothing~;~:*~{~s~^, ~:_~}~]~;~:>"
                     (list (implements module)))
             (format stream "~@:_Claimed Pages: ~<~;~:[None~;~:*~{~a~^, ~:_~}~]~;~:>"
                     (list (module-pages module)))
             (format stream "~@:_API Endpoints: ~<~;~:[None~;~:*~{~(~a~)~^, ~:_~}~]~;~:>"
                     (list (module-api-endpoints module)))
             (format stream "~@:_Configuration: ~<~;~:[None~;~:*~{~s~^, ~:_~}~]~;~:>"
                     (list (let ((table (mconfig-storage module)))
                             (typecase table
                               (hash-table (loop for name being the hash-keys of table collect name))
                               (list table)
                               (T (list table))))))
             (format stream "~@:_Permissions: ~<~;~:[None~;~:*~{~a~^, ~:_~}~]~;~:>"
                     (list (module-permissions module)))
             (format stream "~@:_Hooks: ~<~;~:[None~;~:*~{~a~^, ~:_~}~]~;~:>"
                     (list (list-hooks module)))
             (terpri stream))))))

(defmethod describe-object :after ((module package) stream)
  (when (module-p module)
    (describe-module module stream)))

(defvar *modules-directory* (or #+quicklisp (first ql:*local-project-directories*)
                                #-quicklisp *default-pathname-defaults*))

(defun create-module (name &key (base-file name) dependencies root)
  (let* ((name (string-downcase name))
         (base-file (string-downcase base-file))
         (root (or root (merge-pathnames (make-pathname :directory `(:relative ,name))
                                         *modules-directory*))))
    ;; Create directories
    (ensure-directories-exist root)
    (ensure-directories-exist (merge-pathnames "template/" root))
    (ensure-directories-exist (merge-pathnames "static/" root))

    ;; Populate base ASD
    (with-open-file (s (merge-pathnames (format NIL "~a.asd" name) root) :direction :output)
      (format s "(in-package #:cl-user)~%~
 (asdf:defsystem #:~a
  :version \"0.0.0\"
  :defsystem-depends-on (:radiance)
  :class \"radiance:virtual-module\"
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
      (call ql register-local-projects)
      (call ql quickload (string-upcase name)))
    root))

(defun find-all-modules (directory)
  (loop for asd in (directory (merge-pathnames "**/*.asd" directory))
        for sys = (asdf:find-system (pathname-name asd) NIL)
        when (typep sys 'radiance:virtual-module)
        collect (asdf:component-name sys)))
