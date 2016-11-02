#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

;;; Formats
(defvar *api-formats* (make-hash-table :test 'equalp))
(defvar *default-api-format*)

(defun api-format (name)
  (gethash (string name) *api-formats*))

(defun (setf api-format) (parse-func name)
  (setf (gethash (string name) *api-formats*) parse-func))

(defun remove-api-format (name)
  (remhash (string name) *api-formats*))

(defun list-api-formats ()
  (loop for name being the hash-keys of *api-formats* collect name))

(defmacro define-api-format (name (argsvar) &body body)
  `(setf (api-format ,(string name))
         (lambda (,argsvar)
           (block ,name
             ,@body))))

(defun api-output (data &key (status 200) (message "Ok.") (format (post/get "data-format")))
  (unless data (error 'api-response-empty))
  (let ((format (or format *default-api-format*)))
    (funcall (or (api-format format)
                 (error 'api-unknown-format :format format))
             (let ((table (make-hash-table :test 'equal)))
               (setf (gethash "status" table) status
                     (gethash "message" table) message
                     (gethash "data" table) data)
               table))))

(defgeneric api-serialize (object))

(defmethod no-applicable-method ((func (eql (symbol-function 'api-serialize))) &rest args)
  (declare (ignore func))
  (error 'api-unserializable-object :object (first args)))

;;; Options
(defvar *api-options* (make-hash-table))

(defun api-option (name)
  (gethash name *api-options*))

(defun (setf api-option) (option name)
  (setf (gethash name *api-options*) option))

(defun remove-api-option (name)
  (remhash name *api-options*))

(defun list-api-options ()
  (loop for name being the hash-keys of *api-options* collect name))

(define-options-definer define-api-option api-option (namevar argsvar bodyvar valuevar))

;;; Pages
(defvar *api-pages* (make-hash-table :test 'equalp))

(defun api-page (path)
  (gethash (string path) *api-pages*))

(defun (setf api-page) (page path)
  (setf (gethash (string path) *api-pages*) page))

(defun remove-api-page (path)
  (remhash (string path) *api-pages*))

(defun list-api-pages ()
  (loop for page being the hash-values of *api-pages* collect page))

(defun ensure-api-page (thing)
  (etypecase thing
    (string (api-page thing))
    (symbol (api-page (string thing)))
    (api-page thing)))

(defclass api-page ()
  ((name :initarg :name :accessor name)
   (handler :initarg :handler :reader handler)
   (argslist :initarg :argslist :reader argslist)
   (request-handler :initarg :request-handler :reader request-handler)
   (docstring :initarg :docstring :accessor docstring))
  (:default-initargs
   :name (error "NAME required")
   :handler (error "HANDLER function required")
   :argslist ()
   :request-handler ()
   :docstring NIL))

(defun make-request-calling-function (function &optional (arglist (arg:arglist function)))
  (let ((request (gensym "REQUEST")))
    (flet ((parse (arg optional)
             (cond ((not optional)
                    `(,arg (or* (post/get ,(string arg) ,request) (error 'api-argument-missing :argument ',arg))))
                   ((listp arg)
                    `(,(first arg) (or* (post/get ,(string (first arg)) ,request) ,(second arg))))
                   (T
                    `(,arg (or* (post/get ,(string arg) ,request)))))))
      `(lambda (,request)
         (let* ,(loop with in-optional = NIL
                      for arg in arglist
                      do (when (eql arg '&optional) (setf in-optional T))
                      unless (eql arg '&optional)
                      collect (parse arg in-optional))
           (funcall ,function ,@(extract-lambda-vars arglist)))))))

(defmethod initialize-instance :after ((page api-page) &key)
  (unless (slot-boundp page 'argslist)
    (setf (slot-value page 'argslist)
          (arg:arglist (handler page))))
  (unless (request-handler page)
    (setf (slot-value page 'request-handler)
          (compile NIL (make-request-calling-function (handler page) (argslist page))))))

(defmethod print-object ((api api-page) stream)
  (print-unreadable-object (api stream :type T)
    (format stream "~a ~s" (name api) (argslist api))))

(defmethod documentation ((page api-page) type)
  (docstring page))

(defun call-api-request (api-page &optional (request *request*))
  (let ((*request* request))
    (funcall (request-handler (ensure-api-page api-page))
             request)))

(defun call-api (api-page &rest arguments)
  (apply (handler (ensure-api-page api-page))
         arguments))

(defmacro define-api (name args options &body body)
  (multiple-value-bind (body forms) (expand-options *api-options* options body name args)
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       ,@forms
       ,@(when (module)
           `((pushnew ,(string name) (module-storage ,(module) 'radiance-apis) :test #'equalp)))
       (setf (api-page ,(string name))
             (make-instance
              'api-page
              :name ,(string name)
              :argslist ',args
              :handler (lambda ,args
                         (block ,(when (symbolp name) name)
                           ,@body))
              :docstring ,(getf options :documentation))))))

(define-delete-hook (module 'radiance-destroy-apis)
  (dolist (page (module-storage module 'radiance-apis))
    (remove-api-page page)))

;;; Actual page handler
(define-uri-dispatcher api ("/api/.*" 100)
  (let* ((path (path (uri *request*)))
         (slashpos (position #\/ path))
         (subpath (subseq path (1+ slashpos)))
         (api-page (or (api-page subpath) (api-page ""))))
    (handler-case
        (call-api-request api-page *request*)
      (api-error (err)
        (let ((message (or (message err)
                           (princ-to-string err))))
          (if (string= (post/get "browser") "true")
              (redirect (format NIL "~a?error=~a" (cut-get-part (referer)) message))
              (api-output err :status 500 :message message)))))))
