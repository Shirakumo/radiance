#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(define-admin-panel users verify (:access-branch "admin.verify.users.*" :menu-icon "icon-user" :menu-tooltip "View and manage user accounts" :lquery T)
  (if (string= (post-var "form") "user")
      (progn
        ($ (initialize (template "verify/admin-users-edit.html")))
        (with-model (model _id username displayname register-date secret email perms) ("verify-users" (query (:= "username" (post-var "username"))))
          (when (and (post-var "_id") (= (parse-integer (post-var "_id")) _id))
            (setf displayname (post-var "displayname")
                  register-date (date-to-timestamp (post-var "register-date"))
                  secret (post-var "secret")
                  email (post-var "email")
                  perms (post-var "perms"))
            (model-save model)
            (uibox:notice "User variables updated."))
          ($ "h2" (text (concatenate 'string "Edit User " username)))
          ($ "*[data-uibox]" (each #'(lambda (node) (uibox:fill-node node model :translate-for-input-elements T))))))
      (progn 
        ($ (initialize (template "verify/admin-users-overview.html")))
        (uibox:fill-foreach (model-get T "verify-users" :all :limit -1) "tbody tr"))))

(define-admin-panel actions verify (:access-branch "admin.verify.actions.*" :menu-icon "icon-list-ul" :menu-tooltip "View a list of user actions" :lquery (template "verify/admin-actions.html"))
  (when (string= (post-var "action") "Delete")
    (let ((ids (or (post-var "selected[]")
                   (list (post-var "id")))))
      (dolist (id ids)
        (db-remove T "verify-actions" (query (:= "_id" id)) :limit NIL :skip NIL))
      ($ (prepend (format NIL "<div class=\"notice ok\">Action(s) deleted.</div>")))))
  (uibox:fill-foreach (model-get T "verify-actions" :all :sort '(("time" . :DESC)) :limit -1) "tbody tr"))

(define-admin-panel sessions verify (:access-branch "admin.verify.sessions.*" :menu-icon "icon-signin" :menu-tooltip "See currently active sessions" :lquery (template "verify/admin-session.html"))
  (when (string= (post-var "action") "Deactivate")
    (let ((uuids (or (post-var "selected[]")
                     (list (post-var "uuid")))))
      (loop for uuid in uuids
         for session = (gethash uuid *verify-sessions*)
         if session 
         do (setf (active session) NIL)
         collect uuid into deactivated
         finally (if deactivated
                     (dolist (session deactivated)
                       ($ (prepend (format NIL "<div class=\"notice ok\">Deactivated session ~a</div>" session))))))))

  (uibox:fill-foreach (alexandria:hash-table-values *verify-sessions*) "tbody tr"))

(define-admin-panel authentication verify (:access-branch "admin.verify.authentication.*" :menu-icon "icon-key" :menu-tooltip "Manage authentication mechanisms" :lquery (template "verify/admin-auth.html"))
  (when (string= (post-var "form") "session")
    (setf (config-tree :verify :session :secret) (post-var "secret")
          (config-tree :verify :session :use-per-user-secret) (string= (post-var "per-user") "yes"))
    (uibox:notice "Session settings updated."))

  ($ "input[name=\"secret\"]" (val (config-tree :verify :session :secret)))
  (if (config-tree :verify :session :use-per-user-secret)
      ($ "input[name=\"per-user\"]" (attr :checked "checked")))

  (loop with target = ($ "#mechanisms") 
     for mechanism being the hash-values of *verify-mechanisms*
     do (show-options mechanism target)))

(define-admin-panel registration verify (:access-branch "admin.verify.registration.*" :menu-icon "icon-edit" :menu-tooltip "Manage registration settings" :lquery (template "verify/admin-register.html"))
  (when (string= (post-var "form") "defaults")
    (setf (config-tree :verify :register :endpoint) (post-var "endpoint")
          (config-tree :verify :register :defaultperms) (split-sequence:split-sequence #\Newline (post-var "permissions")))
    (uibox:notice "Registration defaults updated."))
  
  ($ "input[name=\"endpoint\"]" (val (config-tree :verify :register :endpoint)))
  ($ "textarea[name=\"permissions\"]" (text  (concatenate-strings (config-tree :Verify :register :defaultperms) #\Newline))))
