#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *config* (make-hash-table :test 'eql))
(defvar *config-type* :lisp)
(defvar *root* (asdf:system-source-directory :radiance))
(defvar *config-path* (merge-pathnames (make-pathname :name "radiance.uc" :type "lisp") *root*))
(defvar *data-path* (merge-pathnames (make-pathname :directory '(:relative "data")) *root*))
(defvar *random-string-characters* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789")
(defconstant +unix-epoch-difference+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun load-config (&optional (path *config-path*))
  (setf *config* (uc:load-configuration path :format *config-type*)
        *config-path* path)
  T)

(defun save-config (&optional (path *config-path*))
  (uc:save-configuration path :format *config-type* :object *config*)
  T)

(defun config-tree (&rest branches)
  (let ((uc:*config* *config*))
    (apply #'uc:config-tree branches)))

(defun (setf config-tree) (value &rest branches)
  (let ((uc:*config* *config*))
    (apply #'(setf uc:config-tree) value branches)))

(defun make-keyword (name)
  (let ((name (string name)))
    (or (find-symbol name "KEYWORD")
        (intern name "KEYWORD"))))

(declaim (inline concatenate-strings))
(defun concatenate-strings (list &optional (delim ""))
  "Joins a list of strings into one string using format."
  (format nil (format nil "~~{~~A~~^~a~~}" delim) list))

(declaim (inline universal-to-unix-time))
(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(declaim (inline unix-to-universal-time))
(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(declaim (inline get-unix-time))
(defun get-unix-time ()
  "Returns a unix timestamp."
  (universal-to-unix-time (get-universal-time)))

(defun make-random-string (&optional (length 16) (chars *random-string-characters*))
  "Generates a random string of alphanumerics."
  (loop with string = (make-array length :element-type 'character)
        with charlength = (length chars)
        for i from 0 below length
        do (setf (aref string i) (aref chars (random charlength)))
        finally (return string)))

(defun file-size (pathspec)
  "Retrieves the file size in bytes."
  (with-open-file (in pathspec :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun read-data-file (pathspec &key (if-does-not-exist :ERROR))
  "Returns the file contents in string format. Any path is relative to the radiance data directory."
  (with-open-file (stream (merge-pathnames pathspec *data-path*) :if-does-not-exist if-does-not-exist)
    (with-output-to-string (string)
      (let ((buffer (make-array 4096 :element-type 'character)))
        (loop for bytes = (read-sequence buffer stream)
              do (write-sequence buffer string :start 0 :end bytes)
              while (= bytes 4096))))))

(defun data-file (pathname &optional (default *data-path*))
  (merge-pathnames pathname default))

(uc:define-string-deserializer (#\d namestring)
  (data-file (uiop:parse-native-namestring (subseq namestring 1))))

(defun resolve-base (thing)
  (etypecase thing
    (pathname thing)
    (null (resolve-base *package*))
    ((or string symbol package)
     (asdf:system-source-directory
      (modularize:virtual-module
       (modularize:module-identifier thing))))))

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

(defun read-value ()
  (eval (read)))

(defun or* (&rest vals)
  "Functional equivalent of OR with the twist that empty strings are seen as NIL."
  (loop for val in vals
        when (if (stringp val)
                 (not (string= val ""))
                 val)
        do (return val)))

(defun format-universal-time (ut)
  (format NIL "~:@{~4,'0d.~2,'0d.~2,'0d ~2,'0d:~2,'0d:~2,'0d~}"
          (subseq (nreverse (multiple-value-list (decode-universal-time ut))) 3)))

(defun cut-get-part (url)
  (subseq url 0 (position #\? url)))

(defun %static-file (namestring base)
  (merge-pathnames namestring (merge-pathnames "static/" (merge-pathnames (resolve-base base)))))

(defun static-file (namestring &optional base)
  (%static-file namestring base))

(define-compiler-macro static-file (namestring &optional (base *package*))
  (typecase base
    ((or package string)
     (if (stringp namestring)
         (%static-file namestring base)
         `(merge-pathnames ,namestring ,(merge-pathnames "static/" (merge-pathnames (resolve-base base))))))
    (T `(%static-file ,namestring ,base))))

(defun %template (namestring base)
  (merge-pathnames namestring (merge-pathnames "template/" (merge-pathnames (resolve-base base)))))

(defun template (namestring &optional base)
  (%template namestring base))

(define-compiler-macro template (namestring &optional (base *package*))
  (typecase base
    ((or package string)
     (if (stringp namestring)
         (%template namestring base)
         `(merge-pathnames ,namestring ,(merge-pathnames "template/" (merge-pathnames (resolve-base base))))))
    (T `(template ,namestring ,base))))

(defmacro perm (&rest tree)
  "Returns the entered permission TREE as a dotted string and automatically enters it into the current module.
This macro is only usable from within a module context."
  (let ((perm (format NIL "~{~(~a~)~^.~}" tree)))
    ;; Execute during compile
    (pushnew perm (permissions (module)) :test #'string=)
    ;; And ensure during load as well.
    `(load-time-value
      (progn
        (pushnew ,perm (permissions ,(module)) :test #'string=)
        ,perm))))
