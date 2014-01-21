#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-menu)

(defapi admin (action) (:access-branch "admin.menu.*")
  (string-case:string-case ((server:post "action"))
    ("Add"
     (with-model (model pid title tooltip link sort) ("trivial-menu" NIL)
       (setf pid (parse-integer (server:post "pid"))
             title (server:post "title")
             tooltip (server:post "tooltip")
             link (server:post "link")
             sort (parse-integer (server:post "sort")))
       (dm:insert model))
     (init-menu)
     (server:redirect "/menu/items"))
    ("Delete"
     (let ((selected (or (server:post "selected[]") (list (server:post "id")))))
       (dolist (id selected)
         (db:remove "trivial-menu" (db:query (:= "_id" id)) :limit NIL)))
     (init-menu)
     (server:redirect "/menu/items"))
    ("Edit"
     (assert (not (null (server:post "id"))) () 'api-args-error :text "ID argument required.")
     (with-model (model pid title tooltip link sort) ("trivial-menu" (db:query (:= "_id" (server:post "id"))) :save T)
       (setf pid (parse-integer (server:post "pid"))
             title (server:post "title")
             tooltip (server:post "tooltip")
             link (server:post "link")
             sort (parse-integer (server:post "sort"))))
     (init-menu)
     (server:redirect "/menu/items"))))
    

(admin:define-panel items menu (:access-branch "admin.menu.*" :menu-icon "icon-external-link-sign" :menu-tooltip "Change menu items." :lquery (template "trivial-menu/admin.html"))
  (uibox:fill-foreach (dm:get "trivial-menu" :all :sort '(("sort" . :ASC)) :limit -1) "tbody #template"))
