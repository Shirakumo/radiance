#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(admin:define-panel general plaster (:lquery (template "plaster/admin-general.html") :menu-icon "fa-file-text" :menu-tooltip "General plaster settings")
  (cond
    ((string= (server:post "action") "Submit")
     (setf (config-tree :plaster :anon) (not (null (server:post "anon")))
           (config-tree :plaster :captcha) (not (null (server:post "captcha")))
           (config-tree :plaster :maxpastes) (server:post "maxpastes")
           (config-tree :plaster :cooldown) (server:post "cooldown")))
    ((string= (server:post "action") "Add")
     (db:insert "plaster-types" `(("title" . ,(server:post "title")) ("name" . ,(server:post "name")))))
    ((string= (server:post "action") "Delete")
     (dolist (id (or (server:post "selected[]") (list (server:post "id"))))
       (db:remove "plaster-types" (db:query (:= "_id" id))))))
  
  
  ($ "input[name=\"anon\"]" (attr :checked (if (config-tree :plaster :anon) "checked")))
  ($ "input[name=\"captcha\"]" (attr :checked (if (config-tree :plaster :captcha) "checked")))
  ($ "input[name=\"maxpastes\"]" (val (or (config-tree :plaster :maxpastes) "-1")))
  ($ "input[name=\"cooldown\"]" (val (or (config-tree :plaster :cooldown) "0")))

  (uibox:fill-foreach (dm:get "plaster-types" :all :sort '(("title" . :ASC))) "tbody #template"))

(profile:define-panel preferences plaster (:lquery (template "plaster/user-preferences.html") :menu-icon "" :menu-tooltip "")
  )
