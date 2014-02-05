#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(define-interface-method core:define-api (name args (&key (method T) access-branch (identifier `(context-module-identifier))) &body body)
  "Define a new api function.
See interface definition for a description of the arguments.

DEFINE-API works by creating a new hook on the :API namespace
with the hook name format of IDENTIFIER/NAME. The module
identifier is modified into the form of IDENTIFIER:METHOD to 
account for the possibility of multiple-dispatch on different
request methods."
  (assert (find method '(T :GET :POST :PUT :PATCH :DELETE)) () "Method has to be one of T :GET :POST :PUT :PATCH :DELETE")
  (assert (symbolp name) () "Name has to be a symbol.")
  (assert (listp args) () "Args has to be a list.")
  (assert (not (find-any '(&allow-other-keys &body &environment &rest &whole) args))
          () "Only &optional, &key and &aux operators are allowed here.")

  (let* ((argsgens (gensym "ARGUMENTS"))
         (arggens (gensym "ARG"))
         (identgens (gensym "IDENTIFIER"))
         (args (cons '&key (remove-if #'(lambda (a) (find a '(&key &optional))) args)))
         (documentation (if (stringp (car body)) NIL))
         (methodfun (case method (:POST 'server:post) (:GET 'server:get) (otherwise 'server:post-or-get)))
         (function `(apply #'(lambda ,args ,@body)
                           (let ((,argsgens ()))
                             ,@(mapcar #'(lambda (arg)
                                           `(when-let (,arggens (,methodfun ,(string-downcase arg)))
                                              (push ,arggens ,argsgens)
                                              (push ,(make-keyword arg) ,argsgens)))
                                       (extract-lambda-vars args))
                             ,argsgens))))
    `(let ((,identgens ,identifier))
       (v:debug :radiance.server.site "Defining API page ~a for ~a" ',name ,identgens)
       (define-hook (:api (make-keyword (format NIL "~a/~a" ,identgens ',name))) (:identifier (make-keyword (format NIL "~a:~a" ,identgens ,method)) :documentation ,documentation)
         ,@(loop for arg in args until (lambda-keyword-p arg)
                 collect `(assert (not (null (,methodfun ,(string-downcase arg))))
                                  () 'api-args-error :module ,identgens :apicall ',name :text (format NIL "Argument ~a required." ',arg)))
         ,(when access-branch
            `(progn (ignore-errors (auth:authenticate))
                    (assert (authorized-p ,access-branch)
                            () 'api-auth-error :module ,identgens :apicall ',name :text "Not authorized.")))
         ,function))))

(define-interface-method core:api-return (code text &key data)
  (plist->hash-table :CODE code :TEXT text :TIME (get-unix-time) :DATA data))

(define-interface-method core:api-format (format data)
  (let ((format (gethash format *radiance-api-formats*)))
    (if format
        (progn
          (server:set-content-type (second format))
          (funcall (third format) data))
        (api-format :none NIL))))

(define-interface-method core:define-api-format (name content-type datavar &body body)
  (let ((name (make-keyword name)))
    `(progn
       (v:debug :radiance.server.site "Defining new api format ~a, ~a" ,name ,content-type)
       (setf (gethash ,name *radiance-api-formats*)
             (list ,name ,content-type
                   (lambda (,datavar) ,@body))))))

(core::m-define-api-format :trivial-core none "text/plain; charset=utf-8" data
  (declare (ignore data))
  "Unknown format.")

(core::m-define-api-format :trivial-core json "application/json" data
  (cl-json:encode-json-to-string data))

(core::m-define-api-format :trivial-core plist "text/x-lisp-plist" data
  (write-to-string data))

(core::m-define-page :trivial-core api #u"/api/" ()
  (server:set-content-type "none")
  (let* ((pathparts (split-sequence:split-sequence #\/ (path *radiance-request*)))
         (format (make-keyword (string-upcase (or (server:get "format") (server:post "format") "json"))))
         (value  (handler-case 
                     (case (length pathparts)
                       ((1 2) (api-return 200 (format NIL "Radiance API v~a" (asdf:component-version (context-module)))
                                          (plist->hash-table :VERSION (asdf:component-version (context-module)))))
                       (otherwise
                        (let* ((module (cadr pathparts))
                               (trigger (make-keyword (string-upcase (concatenate-strings (cdr pathparts) "/"))))
                               (hooks (hook-items :api trigger)))
                          (or (call-api module hooks)
                              (api-return 204 "No return data")))))
                   (api-args-error (c)
                     (api-return 400 "Invalid arguments"
                                 (plist->hash-table :errortype (class-name (class-of c)) 
                                                    :code (slot-value c 'code)
                                                    :text (slot-value c 'text))))
                   (api-error (c)
                     (api-return 500 "Api error"
                                 (plist->hash-table :errortype (class-name (class-of c))
                                                    :code (slot-value c 'code)
                                                    :text (slot-value c 'text)))))))
    (if (string-equal (server:content-type) "none")
        (api-format format value)
        value)))

(defun identifier-and-method (item-identifier)
  (let* ((item-identifier (string item-identifier))
         (colonpos (position #\: item-identifier)))
    (if colonpos
        (values (subseq item-identifier 0 colonpos)
                (subseq item-identifier (1+ colonpos)))
        (values item-identifier NIL))))

(defun identifier-matches-p (item-identifier identifier &optional (method (server:request-method)))
  (multiple-value-bind (item-identifier item-method) (identifier-and-method item-identifier)
    (and (string-equal item-identifier identifier)
         (or (not item-method)
             (string-equal item-method "T")
             (string-equal item-method (string method))))))

(defun call-api (module hook-items)
  (loop with return = ()
     with accepted = NIL
     for item in hook-items
     if (identifier-matches-p (string (item-identifier item)) module)
     do (setf accepted T)
       (appendf return (funcall (item-function item)))
     finally (return (if accepted
                         return
                         (api-return 404 "Call not found")))))
