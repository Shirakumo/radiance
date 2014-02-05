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
