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
           (config-tree :plaster :maxpastes) (parse-integer (server:post "maxpastes"))
           (config-tree :plaster :cooldown) (parse-integer (server:post "cooldown"))))
    ((string= (server:post "action") "Add")
     (let ((table (cond ((string= (server:post "form") "types") "plaster-types")
                        ((string= (server:post "form") "themes") "plaster-themes"))))
       (db:insert table `(("title" . ,(server:post "title"))
                          ("name" . ,(server:post "name"))
                          ("mime" . ,(server:post "mime"))))))
    ((string= (server:post "action") "Delete")
     (let ((table (cond ((string= (server:post "form") "types") "plaster-types")
                        ((string= (server:post "form") "themes") "plaster-themes"))))
       (dolist (id (or (server:post "selected[]") (list (server:post "id"))))
         (db:remove table (db:query (:= "_id" id)))))))
  
  ($ "input[name=\"anon\"]" (attr :checked (if (config-tree :plaster :anon) "checked")))
  ($ "input[name=\"captcha\"]" (attr :checked (if (config-tree :plaster :captcha) "checked")))
  ($ "input[name=\"maxpastes\"]" (val (or (config-tree :plaster :maxpastes) "-1")))
  ($ "input[name=\"cooldown\"]" (val (or (config-tree :plaster :cooldown) "0")))

  (uibox:fill-foreach (dm:get "plaster-types" :all :sort '(("title" . :ASC))) "#types tbody tr")
  (uibox:fill-foreach (dm:get "plaster-themes" :all :sort '(("title" . :ASC))) "#themes tbody tr"))

(profile:define-panel preferences plaster (:lquery (template "plaster/user-preferences.html") :menu-icon "" :menu-tooltip "")
  (let* ((username (user:field (user:current) "username"))
         (prefs (dm:get-one "plaster-user" (db:query (:= "user" username)))))
    (when (server:get "notice")
      (uibox:notice (server:get "notice")))
    
    (uibox:fill-foreach (dm:get "plaster-themes" :all :sort '(("title" . :ASC))) "#theme option")
    (uibox:fill-foreach (dm:get "plaster-types" :all :sort '(("title" . :ASC))) "#type option")

    ($ (inline (format NIL "#theme option[value=\"~a\"]" (if prefs (dm:field prefs "theme") "default"))) (attr :selected "selected"))
    ($ (inline (format NIL "#type option[value=\"~a\"]" (if prefs (dm:field prefs "default-type") "text"))) (attr :selected "selected"))))
