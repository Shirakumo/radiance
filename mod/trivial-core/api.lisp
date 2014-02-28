#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *apis* (make-hash-table :test #'equal))
;; path => ((method function access)*)
(defvar *formats* (make-hash-table) "Map of all API output formats.")

(defun add-api-function (path method function access)
  (assert (functionp function) () "Function has to be a function object.")
  (assert (or (null access) (stringp access)) () "Access has to be a string or NIL.")
  (assert (find method '(T :GET :POST :PUT :PATCH :DELETE)) () "Method has to be one of T :GET :POST :PUT :PATCH :DELETE")
  (setf path (string-downcase path))
  (let ((methodlist (or (gethash path *apis*) (setf (gethash path *apis*) ()))))
    (let ((position (position method methodlist :key #'first))
          (method (list method function access)))
      (if position
          (progn
            (v:debug :radiance.core.api "Redefining ~a ~a" path method)
            (setf (nth position methodlist) method))
          (progn
            (v:debug :radiance.core.api "Defining new API method ~a ~a" path method)
            (appendf (gethash path *apis*) (list method)))))))

(defun find-api-method (path method)
  (setf path (string-downcase path))
  (let ((methodlist (gethash path *apis*)))
    (or (find method methodlist :key #'car)
        (find T methodlist :key #'car))))

(define-interface-method core:define-api (name args (&key (method T) access-branch (identifier `(context-module-identifier))) &body body)
  "Define a new api function.
See interface definition for a description of the arguments.

DEFINE-API works by creating a new hook on the :API namespace
with the hook name format of IDENTIFIER/NAME. The module
identifier is modified into the form of IDENTIFIER:METHOD to 
account for the possibility of multiple-dispatch on different
request methods."
  
  (assert (symbolp name) () "Name has to be a symbol.")
  (assert (listp args) () "Args has to be a list.")
  (assert (not (find-any '(&allow-other-keys &body &environment &rest &whole) args))
          () "Only &optional, &key and &aux operators are allowed here.")
  (assert (find method '(T :GET :POST :PUT :PATCH :DELETE)) () "Method has to be one of T :GET :POST :PUT :PATCH :DELETE")
  (when-let ((position (position '&optional args)))
    (setf (nth position args) '&key))
  `(add-api-function (format NIL "~a/~a" ,identifier ',name) ,method #'(lambda ,args ,@body) ,access-branch))

(define-interface-method core:api-return (code text &key data)
  (plist->hash-table :CODE code :TEXT text :TIME (get-unix-time) :DATA data))

(define-interface-method core:api-format (format data)
  (let ((format (gethash format *formats*)))
    (if format
        (progn
          (server:set-content-type (second format))
          (funcall (third format) data))
        (core::i-api-format :trivial-core :none NIL))))

(define-interface-method core:define-api-format (name content-type datavar &body body)
  (let ((name (make-keyword name)))
    `(progn
       (v:debug :radiance.server.site "Defining new api format ~a, ~a" ,name ,content-type)
       (setf (gethash ,name *formats*)
             (list ,name ,content-type
                   (lambda (,datavar) ,@body))))))


(define-interface-method core:api-call (apicall method &rest vars)
  (let ((call (find-api-method apicall method)))
    (unless call
      (error 'api-no-call-error :apicall apicall :text "No matching apicall found!"))
    (when (third call)
      (unless (user:check (third call) :user (user:current :authenticate T))
        (error 'api-auth-error :apicall apicall :text "Not authorized to execute this api call.")))
    (or (apply (second call) vars)
        (core:api-return 204 "No return data"))))

(core::m-define-api-format :trivial-core none "text/plain; charset=utf-8" data
  (declare (ignore data))
  "Unknown format.")

(core::m-define-api-format :trivial-core json "application/json" data
  (cl-json:encode-json-to-string data))

(core::m-define-api-format :trivial-core plist "text/x-lisp-plist" data
  (write-to-string data))

(defun generate-api-arguments-list (path lambda-list method)
  (loop with retrieval-method = (case method
                                  (:GET #'server::i-get)
                                  (:POST #'server::i-post)
                                  (otherwise #'server::i-post-or-get))
        with arguments = ()
        with in-keywords = NIL
        for arg in lambda-list
        do (if (eql arg '&key)
               (setf in-keywords T)
               (let ((val (funcall retrieval-method server:*implementation* (string-downcase arg))))
                 (if in-keywords
                     (when val
                       (push (find-symbol (string arg) :KEYWORD) arguments)
                       (push val arguments))
                     (if val
                         (push val arguments)
                         (error 'api-args-error :apicall path :text (format NIL "Argument ~a required, but not set." arg))))))
        finally (return (nreverse arguments))))

(core::m-define-page :trivial-core api #u"/api/" ()
  (server:set-content-type "none")
  (let* ((pathparts (split-sequence:split-sequence #\/ (path *radiance-request*) :remove-empty-subseqs T))
         (path (concatenate-strings (cdr pathparts) "/"))
         (format (make-keyword (string-upcase (or (server:get "format") (server:post "format") "json"))))
         (value (handler-case 
                    (or (when (< (length pathparts) 2)
                          (core:api-return 200 (format NIL "Radiance API v~a" (asdf:component-version (context-module)))
                                           :data (plist->hash-table :VERSION (asdf:component-version (context-module)))))
                        (when-let ((call (find-api-method path (server:request-method))))
                          (or
                           (when (third call)
                             (unless (user:check (third call) :user (user:current :authenticate T))
                               (core:api-return 403 "Not authorized")))
                           (let* ((lambda-list (flatten-lambda-list (sb-introspect:function-lambda-list (second call))))
                                  (arguments (generate-api-arguments-list path lambda-list (server:request-method))))
                             (apply (second call) arguments))
                           (core:api-return 204 "No return data")))
                        (core:api-return 404 "Call not found"))
                  (api-args-error (c)
                    (core:api-return 400 "Invalid arguments"
                                     :data (plist->hash-table :errortype (class-name (class-of c)) 
                                                              :code (slot-value c 'code)
                                                              :text (slot-value c 'text))))
                  (api-error (c)
                    (core:api-return 500 "Api error"
                                     :data (plist->hash-table :errortype (class-name (class-of c))
                                                              :code (slot-value c 'code)
                                                              :text (slot-value c 'text)))))))
    (if (string-equal (server:content-type) "none")
        (core:api-format format value)
        value)))
