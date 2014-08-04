#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

;;; Formats
(defvar *api-formats* (make-hash-table :test 'equalp))
(defvar *default-api-format* "lisp")

(defun api-format (name)
  (gethash (string name) *api-formats*))

(defun (setf api-format) (parse-func name)
  (setf (gethash (string name) *api-formats*) parse-func))

(defun remove-api-format (name)
  (remhash (string name) *api-formats*))

(defmacro define-api-format (name (argsvar) &body body)
  `(setf (api-format ,(string name))
         #'(lambda (,argsvar) ,@body)))

(defun api-output (data)
  (unless data (error 'api-response-empty))
  (let ((format (or (get-var "format") *default-api-format*)))
    (funcall (or (api-format format)
                 (error 'api-unknown-format :format format))
             data)))

(defgeneric api-serialize (object))

;;; Options
(defvar *api-options* (make-hash-table))

(defun api-option (name)
  (gethash name *api-options*))

(defun (setf api-option) (option name)
  (setf (gethash name *api-options*) option))

(defun remove-api-option (name)
  (remhash name *api-options*))

(defmacro define-api-option (name (namevar argsvar &rest rest) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (api-option ,(make-keyword name))
           #'(lambda (,namevar ,argsvar ,@rest)
               ,@body))))

;;; Pages
(defvar *api-pages* (make-hash-table :test 'equalp))

(defun api-page (path)
  (gethash (string path) *api-pages*))

(defun (setf api-page) (page path)
  (setf (gethash (string path) *api-pages*) page))

(defun remove-api-page (path)
  (remhash (string path) *api-pages*))

(defclass api-page ()
  ((handler :initarg :handler :initform (error "HANDLER function required.") :accessor handler)
   (argslist :initarg :argslist :initform () :accessor argslist)
   (docstring :initarg :docstring :initform NIL :accessor docstring)))

(defun api-call (api-page request)
  (loop with args = ()
        with in-optional = NIL
        for arg in (argslist api-page)
        do (cond
             ((eql arg '&optional)
              (setf in-optional T))
             ((and in-optional (listp arg))
              (let ((val (post/get (string (first arg)) request)))
                (push (or val (second arg)) args)))
             (in-optional
              (push (post/get (string arg) request) args))
             (T
              (let ((val (post/get (string arg) request)))
                (if val (push val args) (error 'api-argument-missing :argument arg)))))
        finally (apply (handler api-page) (nreverse args))))

(defvar *api-body*)
(defmacro define-api (name args options &body body)
  (let ((*api-body* body)
        (no-value (gensym "NO-VALUE")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@(loop for option being the hash-keys of *api-options*
               for function being the hash-values of *api-options*
               for value = (getf options option no-value)
               for result = (if (eql value no-value)
                                (funcall function name args)
                                (funcall function name args value))
               when result
                 collect result)
       (setf (api-page ,(string name))
             (make-instance
              'api-page
              :argslist ',args
              :handler #'(lambda ,(extract-lambda-vars args) ,@*api-body*)
              :docstring ,(getf options :documentation))))))

;;; Actual page handler
(define-page api #u"/api/.*" ()
  (let* ((slashpos (position #\/ (path *request*)))
         (subpath (subseq (path *request*) (1+ slashpos)))
         (api-page (or (api-page subpath) (api-page ""))))
    (handler-case
        (api-output (api-call api-page *request*))
      (api-error (err)
        (api-output err)))))
