#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(admin:define-panel users verify (:access-branch "admin.verify.users.*" :menu-icon "icon-user" :menu-tooltip "View and manage user accounts" :lquery T)
  (if (string= (server:post "form") "user")
      (progn
        ($ (initialize (template "verify/admin-users-edit.html")))
        (with-model (model _id username displayname register-date secret email perms) ("verify-users" (db:query (:= "username" (server:post "username"))))
          (when (and (server:post "_id") (= (parse-integer (server:post "_id")) _id))
            (setf displayname (server:post "displayname")
                  register-date (date-to-timestamp (server:post "register-date"))
                  secret (server:post "secret")
                  email (server:post "email")
                  perms (server:post "perms"))
            (dm:save model)
            (uibox:notice "User variables updated."))
          ($ "h2" (text (concatenate 'string "Edit User " username)))
          ($ "*[data-uibox]" (each #'(lambda (node) (uibox:fill-node node model))))))
      (progn 
        ($ (initialize (template "verify/admin-users-overview.html")))
        (uibox:fill-foreach (dm:get "verify-users" :all :limit -1) "tbody tr"))))

(admin:define-panel actions verify (:access-branch "admin.verify.actions.*" :menu-icon "icon-list-ul" :menu-tooltip "View a list of user actions" :lquery (template "verify/admin-actions.html"))
  (when (string= (server:post "action") "Delete")
    (let ((ids (or (server:post "selected[]")
                   (list (server:post "id")))))
      (dolist (id ids)
        (db:remove "verify-actions" (db:query (:= "_id" id)) :limit NIL :skip NIL))
      ($ (prepend (format NIL "<div class=\"notice ok\">Action(s) deleted.</div>")))))
  (uibox:fill-foreach (dm:get "verify-actions" :all :sort '(("time" . :DESC)) :limit -1) "tbody tr"))

(admin:define-panel sessions verify (:access-branch "admin.verify.sessions.*" :menu-icon "icon-signin" :menu-tooltip "See currently active sessions" :lquery (template "verify/admin-session.html"))
  (when (string= (server:post "action") "Deactivate")
    (let ((uuids (or (server:post "selected[]")
                     (list (server:post "uuid")))))
      (loop for uuid in uuids
         for session = (gethash uuid *verify-sessions*)
         if session 
         do (setf (active session) NIL)
         collect uuid into deactivated
         finally (if deactivated
                     (dolist (session deactivated)
                       ($ (prepend (format NIL "<div class=\"notice ok\">Deactivated session ~a</div>" session))))))))

  (uibox:fill-foreach (alexandria:hash-table-values *verify-sessions*) "tbody tr"))

(admin:define-panel authentication verify (:access-branch "admin.verify.authentication.*" :menu-icon "icon-key" :menu-tooltip "Manage authentication mechanisms" :lquery (template "verify/admin-auth.html"))
  (when (string= (server:post "form") "session")
    (setf (config-tree :verify :session :secret) (server:post "secret")
          (config-tree :verify :session :use-per-user-secret) (string= (server:post "per-user") "yes"))
    (uibox:notice "Session settings updated."))

  ($ "input[name=\"secret\"]" (val (config-tree :verify :session :secret)))
  (if (config-tree :verify :session :use-per-user-secret)
      ($ "input[name=\"per-user\"]" (attr :checked "checked")))

  (loop with target = ($ "#mechanisms") 
     for mechanism being the hash-values of *verify-mechanisms*
     do (show-options mechanism target)))

(admin:define-panel registration verify (:access-branch "admin.verify.registration.*" :menu-icon "icon-edit" :menu-tooltip "Manage registration settings" :lquery (template "verify/admin-register.html"))
  (when (string= (server:post "form") "defaults")
    (setf (config-tree :verify :register :endpoint) (server:post "endpoint")
          (config-tree :verify :register :defaultperms) (split-sequence:split-sequence #\Newline (server:post "permissions")))
    (uibox:notice "Registration defaults updated."))
  
  ($ "input[name=\"endpoint\"]" (val (config-tree :verify :register :endpoint)))
  ($ "textarea[name=\"permissions\"]" (text  (concatenate-strings (config-tree :Verify :register :defaultperms) #\Newline))))
