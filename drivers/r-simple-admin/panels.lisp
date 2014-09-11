#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:simple-admin)

(admin:define-panel overview admin (:icon "fa-home")
  (r-clip:process
   (plump:parse (template "overview.ctml"))))

(admin:define-panel modules admin (:icon "fa-cube")
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

(admin:define-panel systems admin (:icon "fa-briefcase")
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
