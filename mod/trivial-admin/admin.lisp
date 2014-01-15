#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-admin)

(defvar *menu* ())
(defvar *categories* (make-hash-table))

(defun make-keyword (name)
  (setf name (string-upcase name))
  (or (find-symbol name :keyword)
      (intern name :keyword)))

(defpage admin #u"admin./" (:lquery (template "admin/index.html") :access-branch "admin.*")
  (uibox:fill-foreach *menu* "#panel>ul>li")
  (let ((pathparts (split-sequence:split-sequence #\/ (string-downcase (path *radiance-request*)))))
    (if (< (length pathparts) 2) (setf pathparts (list "core" "index")))
    ($ (find (format NIL "a[href=\"/~a/~a\"]" (first pathparts) (second pathparts))) (parent) (add-class "active"))
    
    (let ((category (gethash (make-keyword (first pathparts)) *categories*)))
      (if category 
          (let ((inf (gethash (make-keyword (second pathparts)) category)))
            (if (and inf (first inf))
                ($ "#content" (append (funcall (first inf))))))))))

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

(define-interface-method admin:define-panel (name category options &rest body)
  (destructuring-bind (&key lquery access-branch menu-icon menu-tooltip) options
    (let* ((name (make-keyword name))
           (category (make-keyword category))
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
                          (if (authorized-p ,access-branch)
                              ,funcbody
                              (error-page 403)))
                       funcbody))
                ,menu-icon
                ,menu-tooltip))
         (build-menu)))))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (when (or (eql (admin:implementation) :trivial-admin)
            (not (admin:implementation)))
    (admin:implementation :trivial-admin)

    (admin:define-panel index core (:menu-icon "icon-home" :menu-tooltip "Index" :lquery (template "admin/panel-index.html"))
      )

    (admin:define-panel modules core (:menu-icon "icon-tasks" :menu-tooltip "Manage radiance modules" :lquery (template "admin/panel-modules.html"))
      (uibox:fill-foreach (alexandria:hash-table-values *radiance-modules*) "tbody tr"))

    (admin:define-panel hooks core (:menu-icon "icon-random" :menu-tooltip "Manage triggers and hooks" :lquery (template "admin/panel-hooks.html"))
      (uibox:fill-foreach
       (loop for space being the hash-keys of *radiance-hooks*
          for hooks being the hash-values of *radiance-hooks*
          collect (list :space space :hooks (loop for hook being the hash-keys of hooks
                                               for triggers being the hash-values of hooks
                                               collect (list :hook hook :triggers triggers))))
       "#spaces .tablebox"))))
  
