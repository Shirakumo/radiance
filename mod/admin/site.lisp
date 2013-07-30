#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-admin)

(implement 'admin (get-module 'radiance-admin))

(defpage site ()
  (authenticate (implementation 'auth))
  (if (authorized-p "admin.*")
      (multiple-value-bind (menu active) (build-menu admin)
        ($ (initialize (template "admin/index.html")))
        (uibox:fill-foreach menu "#panel ul")
        (let ((*lquery-master-document* (first ($ "#content"))))
          (apply (car active) (cdr active)))
        (first ($ (serialize))))
      (error-page 403)))

(defun build-menu (admin)
  (loop with path = (path *radiance-request*)
     with active = NIL
     for category being the hash-keys of (categories admin)
     for info being the hash-values of (categories admin)
     collect `(:category ,category 
               :icon ,(getf info :icon)
               :panels ,(loop for (name func) on (getf info :panels) by #'cddr
                           collect (let ((link (format nil "/~a/~a" category name)))
                                     `(:panel ,(symbol-name name)
                                       :link ,link
                                       :class ,(when (and (>= (length path) (length link))
                                                          (string-equal path link :end1 (length link)))
                                                     (setf active func) 
                                                     "active"))))) into menu
     finally (return (values menu active))))

(defmethod admin-category ((admin radiance-admin) category &key (icon "icon-folder-close") &allow-other-keys)
  (setf (gethash category (categories admin)) `(:icon ,icon)))

(defmethod admin-panel ((admin radiance-admin) name category function &key funcargs &allow-other-keys)
  (assert (gethash category (categories admin)) () "Category ~a is undefined!" category)
  (setf (getf (getf (gethash category (categories admin)) :panels) (make-keyword name))
        (cons function funcargs)))

(defun admin-index ()
  )

(defun admin-modules ()
  ($ (html (read-data-file "template/admin/panel-modules.html")))
  ())

(defun admin-triggers ()
  ())

(admin-category (get-module 'radiance-admin) "Core" :icon "icon-gears")
(admin-panel (get-module 'radiance-admin) "Index" "Core" #'admin-index)
(admin-panel (get-module 'radiance-admin) "Modules" "Core" #'admin-modules)
(admin-panel (get-module 'radiance-admin) "Triggers" "Core" #'admin-triggers)
