#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:simple-admin)

(defpackage #:noop (:use))
(defun print-symbol (symb)
  (let ((*package* (find-package "NOOP")))
    (prin1-to-string symb)))

;; We need to defer here as well to hope that the other hooks take place
(define-implement-hook admin
  (admin:define-panel overview admin (:access (radiance admin) :icon "fa-home" :tooltip "Radiance overview info.")
    (r-clip:process
     (plump:parse (template "overview.ctml"))))

  (admin:define-panel modules admin (:access (radiance admin modules) :icon "fa-cube" :tooltip "Oversee active modules.")
    (let* ((action (post-var "action"))
           (selected (post-var "selected[]"))
           (module (post-var "module"))
           (modules (if module (cons module selected) selected))
           (error NIL) (info NIL))
      (when (string-equal action "delete")
        (handler-case
            (progn (dolist (module modules)
                     (l:warn :simple-admin "Deleting module ~a as per front-end request." module)
                     (modularize:delete-module module))
                   (setf info (format NIL "Deleted modules ~{~a~^, ~}" modules)))
          (error (err)
            (setf error (princ-to-string err)))))
      
      (r-clip:process
       (plump:parse (template "modules.ctml"))
       :error error
       :info info
       :modules (remove-if #'interfaces:interface-p (modularize:list-modules)))))

  (admin:define-panel systems admin (:access (radiance admin systems) :icon "fa-briefcase" :tooltip "Manage ASDF systems.")
    (let* ((action (post-var "action"))
           (selected (post-var "selected[]"))
           (system (post-var "system"))
           (systems (if system (cons system selected) selected))
           (error NIL) (info NIL))
      (handler-case
          (cond ((string-equal action "reload")
                 (dolist (system systems)
                   (l:info :simple-admin "Reloading system ~a as per front-end request." system)
                   (asdf:load-system system))
                 (setf info (format NIL "Reloaded systems ~{~a~^, ~}" systems)))
                ((string-equal action "load")
                 (l:info :simple-admin "Attempting to load system ~a as per front-end request." system)
                 (asdf:load-system system)
                 (setf info (format NIL "System ~a loaded." system)))
                ((string-equal action "quickload")
                 (l:info :simple-admin "Attempting to quickload system ~a as per front-end request." system)
                 (ql:quickload system)
                 (setf info (format NIL "System ~a quickloaded." system))))
        (error (err)
          (setf error (princ-to-string err))))
      
      (r-clip:process
       (plump:parse (template "systems.ctml"))
       :error error
       :info info
       :systems (asdf:already-loaded-systems))))

  (admin:define-panel dispatchers admin (:access (radiance admin dispatchers) :icon "fa-at" :tooltip "Manage Radiance's dispatchers.")
    (let* ((action (post-var "action"))
           (selected (post-var "selected[]"))
           (dispatcher (post-var "dispatcher"))
           (dispatchers (if dispatcher (cons dispatcher selected) selected))
           (error NIL) (info NIL))
      (handler-case
          (cond ((string-equal action "remove")
                 (dolist (dispatcher dispatchers)
                   (let ((dispatcher (let ((*read-eval* NIL))
                                       (read-from-string dispatcher))))
                     (l:info :simple-admin "Removing dispatcher ~s as per front-end request." dispatcher)
                     (remove-uri-dispatcher dispatcher)))
                 (setf info (format NIL "Removed dispatchers ~{~a~^, ~}" dispatchers))))
        (error (err)
          (setf error (princ-to-string err))))
      
      (r-clip:process
       (plump:parse (template "dispatchers.ctml"))
       :error error
       :info info
       :dispatchers (list-uri-dispatchers))))

  (admin:define-panel sessions admin (:access (radiance admin sessions) :icon "fa-tags" :tooltip "Oversee active sessions.")
    (with-actions (error info)
        ((:end
          (dolist (id (or (post-var "selected[]") (list (post-var "id"))))
            (session:end (session:get id)))
          (setf info "Sessions ended.")))
      (r-clip:process
       (plump:parse (template "sessions.ctml"))
       :error error :info info
       :sessions (session:list)))))
