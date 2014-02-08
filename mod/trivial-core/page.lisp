#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)
(define-interface-method core:define-page (name uri options &body body)
  "Define a new page for a given module.
See interface definition for a description of the arguments.

DEFINE-PAGE works by creating a new hook on the :PAGE namespace
with the hook name NAME."
  (assert (symbolp name) () "The name has to be a symbol.")
  (destructuring-bind (&key access-branch lquery (identifier `(context-module-identifier))) options
    (let ((urigens (gensym "URI")) (modgens (gensym "MODULE"))
          (funcbody (if lquery 
                        `(progn 
                           ,(if (and lquery (not (eq lquery T)))
                                `($ (initialize ,lquery)))
                           ,@body
                           (unless (server::content *radiance-response*)
                             (trigger :user :lquery-post-processing)
                             (concatenate-strings ($ (serialize)))))
                        `(progn ,@body))))
      `(let ((,urigens ,uri)
             (,modgens ,identifier))
         (v:debug :radiance.server.site "Defining new site ~a on ~a for ~a" ',name ,urigens ,modgens)
         (define-hook (:page ',name) (:identifier ,modgens :documentation (format nil "Page call for ~a" ,modgens))
           ,(if access-branch
                `(progn
                   (ignore-errors (auth:authenticate))
                   (if (user:check ,access-branch)
                       ,funcbody
                       (error-page 403)))
                funcbody))
         (dispatcher:register ',name ,modgens ,urigens)))))

(define-interface-method core:define-file-link (name uri pathspec &key access-branch (identifier `(context-module-identifier)) content-type)
  `(core::m-define-page :trivial-core ,name ,uri (:identifier ,identifier :access-branch ,access-branch)
     (server:serve-file ,pathspec :content-type ,content-type)))

(core::m-define-file-link :trivial-core favicon #u"/favicon.ico" (static "img/favicon.ico") :content-type "image/x-icon")
(core::m-define-file-link :trivial-core robots #u"/robots.txt" (static "txt/robots.txt") :content-type "text/plain")
(core::m-define-file-link :trivial-core humans #u"/humans.txt" (static "txt/humans.txt") :content-type "text/plain")

(defvar *hooks* ())

(define-interface-method dispatcher:dispatch (request)
  (or
   (let ((hook (dispatcher:effective-trigger request)))
     (when hook
       (v:debug :flash-dispatch "Dispatching to hook: ~a" hook)
       (funcall (item-function hook))))
   (trigger :server :dispatch-default)
   (dispatcher:dispatch-default request)))

(define-interface-method dispatcher:effective-trigger (request)
  (loop for (hook identifier uri) in *hooks*
        if (uri-matches uri request)
          do (return (loop for item in (hook-items :page hook)
                           if (eql identifier (item-identifier item))
                             return item))))

(define-interface-method dispatcher:register (hook identifier uri)
  (let ((found (find uri *hooks* :key #'third :test #'uri-same)))
    (when found
      (if (not (eql hook (first found)))
          (v:warn :flash-dispatch "Overriding existing trigger ~a on ~a" (car found) uri))
      (setf *hooks* (delete uri *hooks* :key #'third :test #'uri-same))))
  (v:debug :flash-dispatch "Registering ~a for ~a/~a" uri identifier hook)
  (setf *hooks* (sort (append *hooks* `((,hook ,identifier ,uri))) #'sort-dispatcher-hooks))
  hook)

(define-interface-method dispatcher:unregister (uri)
  (v:debug :flash-dispatch "Deleting hooks on uri ~a." uri)
  (setf *hooks* (delete uri *hooks* :test #'(lambda (a) (uri-same uri (third a))))))

(define-interface-method dispatcher:dispatch-default (request)
  (error-page 404))

(defun sort-dispatcher-hooks (a b)
  (flet ((path (hook) (path (third hook))))
    (or (> (count #\/ (path a))
           (count #\/ (path b)))
        (> (length (path a))
           (length (path b))))))

