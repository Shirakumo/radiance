#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-menu)

(defapi admin (action) (:access-branch "admin.menu.*")
  (string-case:string-case ((post-var "action"))
    ("Add"
     (with-model (model pid title tooltip link sort) ("trivial-menu" NIL)
       (setf pid (parse-integer (post-var "pid"))
             title (post-var "title")
             tooltip (post-var "tooltip")
             link (post-var "link")
             sort (parse-integer (post-var "sort")))
       (dm:insert model))
     (init-menu)
     (redirect "/menu/items"))
    ("Delete"
     (let ((selected (or (post-var "selected[]") (list (post-var "id")))))
       (dolist (id selected)
         (db:remove "trivial-menu" (db:query (:= "_id" id)) :limit NIL)))
     (init-menu)
     (redirect "/menu/items"))
    ("Edit"
     (assert (not (null (post-var "id"))) () 'api-args-error :text "ID argument required.")
     (with-model (model pid title tooltip link sort) ("trivial-menu" (db:query (:= "_id" (post-var "id"))) :save T)
       (setf pid (parse-integer (post-var "pid"))
             title (post-var "title")
             tooltip (post-var "tooltip")
             link (post-var "link")
             sort (parse-integer (post-var "sort"))))
     (init-menu)
     (redirect "/menu/items"))))
    

(admin:define-panel items menu (:access-branch "admin.menu.*" :menu-icon "icon-external-link-sign" :menu-tooltip "Change menu items." :lquery (template "trivial-menu/admin.html"))
  (uibox:fill-foreach (dm:get "trivial-menu" :all :sort '(("sort" . :ASC)) :limit -1) "tbody #template"))
