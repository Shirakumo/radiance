#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(profile::m-define-panel :trivial-profile profile user (:menu-icon "icon-user" :menu-tooltip "Profile settings" :lquery (template "trivial-profile/panel-profile.html"))
  ($ "*[data-uibox]" (each #'(lambda (node) (uibox:fill-node node (user)))))
  
  (let* ((parent ($ "#fields li" (parent) (node)))
         (template ($ parent "li" (last) (remove) (node)))
         (fields (db:select "trivial-profile" (db:query (:= "user" (user:field (user) "username"))) :limit -1)))
    (db:iterate "trivial-profile-fields" :all
      #'(lambda (row)
          (let ((field (cdr (assoc "field" row :test #'string=)))
                (type (cdr (assoc "type" row :test #'string=)))
                (value (cdr (assoc "balue" row :test #'string=)))
                (clone ($ template (clone) (node))))
            ($ clone ".key" (text field))
            ($ clone ".value" (attr :type type :name field) (val value))
            (loop for vrow in fields
                  if (and (string= field (cdr (assoc "field" vrow :test #'string=))))
                    do ($ clone ".value" (val (cdr (assoc "value" vrow :test #'string=))))
                       (return))
            ($ parent (append clone)))))))

(profile::m-define-panel :trivial-profile comments user (:menu-icon "icon-comments" :menu-tooltip "Manage comments on your profile" :lquery (template "trivial-profile/panel-comments.html"))
  (uibox:fill-foreach (dm:get "trivial-profile-comments" (db:query (:= "user" (user:field (user) "username"))) :sort '(("time" . :DESC)) :limit -1) "tbody tr"))
