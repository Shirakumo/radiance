#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-admin)

(defvar *menu* ())
(defvar *categories* (make-hash-table))

(core:define-page admin #u"admin./" (:lquery (template "admin/index.html") :access-branch "admin.*")
  (uibox:fill-foreach *menu* "#panel>ul>li")
  (let ((pathparts (split-sequence:split-sequence #\/ (string-downcase (path *radiance-request*)))))
    (if (< (length pathparts) 2) (setf pathparts (list "core" "index")))
    ($ (find (format NIL "a[href=\"/~a/~a\"]" (first pathparts) (second pathparts))) (parent) (add-class "active"))
    
    (let ((category (gethash (make-keyword (string-upcase (first pathparts))) *categories*)))
      (if category 
          (let ((inf (gethash (make-keyword (string-upcase (second pathparts))) category)))
            (if (and inf (first inf))
                ($ "#content" (append (funcall (first inf))))))))

    (when-let ((action (server:post "manage"))
               (current-path (path *radiance-request*)))
      (when (user:check (format NIL "radiance.manage.~a" action))
        (uibox:confirm ((format NIL "Do you really want to ~a?" action))
                       (progn (user:action (format NIL "Radiance server manage action: ~a" action))
                              (radiance:manage action)
                              (server:redirect (format NIL "/~a" current-path)))
                       (server:redirect (format NIL "/~a" current-path)))))))

(defun build-menu ()
  (setf *menu*
        (loop for module being the hash-keys of *categories*
           for panels being the hash-values of *categories*
           if (> (hash-table-count panels) 0)
           collect (list :category (string-upcase (string-downcase module) :end 1) :panels (build-panels module panels)))))

(defun build-panels (module panelmap)
  (loop for panel being the hash-keys of panelmap
     for (func icon tooltip) being the hash-values of panelmap
     for link = (string-downcase (format NIL "/~a/~a" module panel))
     collect (list :panel (string-upcase (string-downcase panel) :end 1) :link link :icon (or icon "") :title (or tooltip ""))))

(define-interface-method admin:define-panel (name category (&key lquery access-branch menu-icon menu-tooltip) &rest body)
  (let* ((name (make-keyword (string-upcase name)))
         (category (make-keyword (string-upcase category)))
         (categorygens (gensym "ADMINCAT"))
         (getcategory `(gethash ,category ,categorygens))
         (funcbody (if lquery 
                       `(let ((lquery:*lquery-master-document* NIL))
                          ,(if (and lquery (not (eq lquery T)))
                               `(lquery:$ (initialize ,lquery)))
                          ,@body
                          (concatenate-strings (lquery:$ (serialize :doctype NIL))))
                       `(progn ,@body))))
    `(let ((,categorygens *categories*))
       (unless ,getcategory
         (setf ,getcategory (make-hash-table)))
       (setf (gethash ',name ,getcategory)
             (list 
              (lambda ()
                ,(if access-branch
                     `(progn
                        (ignore-errors (auth:authenticate))
                        (if (user:check ,access-branch)
                            ,funcbody
                            (error-page 403)))
                     funcbody))
              ,menu-icon
              ,menu-tooltip))
       (build-menu))))  
