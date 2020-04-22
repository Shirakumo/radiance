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

(defun api-output (data &rest metadata &key (status 200) (message "Ok.") (format (post/get "data-format")) &allow-other-keys)
  (let ((format (or format *default-api-format*)))
    (funcall (or (api-format format)
                 (error 'api-unknown-format :format format))
             (let ((table (make-hash-table :test 'equal)))
               (setf (gethash "status" table) status
                     (gethash "message" table) message
                     (gethash "data" table) data)
               (loop for (key val) on metadata by #'cddr
                     do (unless (or (eql key :status) (eql key :message) (eql key :format))
                          (setf (gethash (string-downcase key) table) val)))
               table))))

(defgeneric api-serialize (object))

(defmethod no-applicable-method ((func (eql (symbol-function 'api-serialize))) &rest args)
  (declare (ignore func))
  (error 'api-unserializable-object :object (first args)))

;;; Pages
(defvar *api-endpoints* (make-hash-table :test 'equalp))

(defun api-endpoint (path)
  (gethash (string path) *api-endpoints*))

(defun (setf api-endpoint) (page path)
  (setf (gethash (string path) *api-endpoints*) page))

(defun remove-api-endpoint (path)
  (remhash (string path) *api-endpoints*))

(defun list-api-endpoints ()
  (loop for page being the hash-values of *api-endpoints* collect page))

(defun ensure-api-endpoint (thing)
  (etypecase thing
    (string (or (api-endpoint thing)
                (error "No such api-endpoint ~s." thing)))
    (symbol (api-endpoint (string thing)))
    (api-endpoint thing)))

(define-documentable api-endpoint ()
  ((name :initarg :name :accessor name)
   (handler :initarg :handler :accessor handler)
   (argslist :initarg :argslist :accessor argslist)
   (request-handler :initarg :request-handler :accessor request-handler))
  (:default-initargs
   :name (error "NAME required")
   :handler (error "HANDLER function required")
   :argslist ()
   :request-handler ())
  (:find-function api-endpoint))

(defun make-request-handler-function (function &optional (arglist (arg:arglist function)))
  (let ((request (gensym "REQUEST")))
    (flet ((parse (arg optional)
             (cond ((not optional)
                    `(,arg (or* (post/get ,(string arg) ,request) (error 'api-argument-missing :argument ',arg))))
                   ((listp arg)
                    `(,(first arg) (or* (post/get ,(string (first arg)) ,request) ,(second arg))))
                   (T
                    `(,arg (or* (post/get ,(string arg) ,request)))))))
      `(lambda (,request)
         (declare (ignorable ,request))
         (let* ,(loop with in-optional = NIL
                      for arg in arglist
                      do (when (eql arg '&optional) (setf in-optional T))
                      unless (eql arg '&optional)
                      collect (parse arg in-optional))
           (funcall ,function ,@(extract-lambda-vars arglist)))))))

(defmethod initialize-instance :after ((page api-endpoint) &key)
  (unless (slot-boundp page 'argslist)
    (setf (slot-value page 'argslist)
          (arg:arglist (handler page))))
  (unless (request-handler page)
    (setf (slot-value page 'request-handler)
          (ecompile NIL (make-request-handler-function (handler page) (argslist page))))))

(defmethod print-object ((api api-endpoint) stream)
  (print-unreadable-object (api stream :type T)
    (format stream "~a ~s" (name api) (argslist api))))

(defun call-api-request (api-endpoint &optional (request *request*))
  (let ((*request* request))
    (funcall (request-handler (ensure-api-endpoint api-endpoint))
             request)))

(defun call-api (api-endpoint &rest arguments)
  (apply (handler (ensure-api-endpoint api-endpoint))
         arguments))

(defmacro define-api (name args options &body body)
  (let ((handler (gensym "HANDLER")))
    (multiple-value-bind (body forms) (expand-options 'api options name body args)
      `(eval-when (:compile-toplevel :load-toplevel :execute)
         ,@forms
         ,@(when (module)
             `((pushnew ,(string name) (module-storage ,(module) 'radiance-apis) :test #'equalp)))
         (flet ((,handler ,args
                  (block ,(when (symbolp name) name)
                    ,@body)))
           (setf (api-endpoint ,(string name))
                 (make-instance
                  'api-endpoint
                  :name ,(string name)
                  :argslist ',args
                  :handler #',handler
                  :request-handler ,(make-request-handler-function `#',handler args)
                  :documentation ,(form-fiddle:lambda-docstring `(lambda () ,@body)))))))))

(define-delete-hook (module 'radiance-destroy-apis)
  (dolist (page (module-storage module 'radiance-apis))
    (remove-api-endpoint page)))

(defmacro api-error (format-string &rest format-args)
  `(error 'api-error :message (format NIL ,format-string ,@format-args)))

;;; Actual page handler
(define-uri-dispatcher api ("/^api/.*" 100)
  (let* ((path (path (uri *request*)))
         (slashpos (position #\/ path))
         (subpath (subseq path (1+ slashpos)))
         (api-endpoint (or (api-endpoint subpath) (api-endpoint ""))))
    (flet ((output-error (err code)
             (let ((message (or (message err) (princ-to-string err))))
               (if (and (string= (post/get "browser") "true") (referer))
                   (redirect (merge-url (referer) :parameters `(("error" . ,(princ-to-string message)))))
                   (api-output err :status code :message message)))))
      (handler-case
          (restart-case
              (handler-bind ((api-error #'maybe-invoke-debugger))
                (call-api-request api-endpoint *request*))
            (api-error (err)
              :report "Treat as API error."
              (output-error err 500)))
        (api-error (err)
          (output-error err 500))
        (request-not-found (err)
          (output-error err 403))
        (request-denied (err)
          (output-error err 404))))))
