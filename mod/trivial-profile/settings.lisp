#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defpage user-settings #u"user./settings" (:modulevar trivial-profile :lquery (template "trivial-profile/settings.html") :access-branch "user.settings.*")
  (uibox:fill-foreach (menu trivial-profile) "#template")
  ($ "#my-profile" (attr :href (concatenate 'string "/" (user-field (user) "username"))))
  (let ((pathparts (cdr (split-sequence:split-sequence #\/ (string-downcase (path *radiance-request*))))))
    (if (< (length pathparts) 2) (setf pathparts (list "profile" "index")))
    ($ (find (format NIL "a[href=\"/settings/~a/~a\"]" (first pathparts) (second pathparts))) (parent) (add-class "active"))
    
    (let ((category (gethash (make-keyword (first pathparts)) (categories trivial-profile))))
      (if category 
          (let ((inf (gethash (make-keyword (second pathparts)) category)))
            (if (and inf (first inf))
                ($ "#content" (append (funcall (first inf))))))))))

(defun build-menu ()
  (let ((admin (get-module :trivial-profile)))
    (setf (menu (get-module :trivial-profile))
          (loop for module being the hash-keys of (categories admin)
             for panels being the hash-values of (categories admin)
             if (> (hash-table-count panels) 0)
             collect (list :category (string-upcase (string-downcase module) :end 1) :panels (build-panels module panels))))))

(defun build-panels (module panelmap)
  (loop for panel being the hash-keys of panelmap
     for (func icon tooltip) being the hash-values of panelmap
     for link = (string-downcase (format NIL "/settings/~a/~a" module panel))
     collect (list :panel (string-upcase (string-downcase panel) :end 1) :link link :icon (or icon "") :title (or tooltip ""))))

(defmacro define-user-panel (name category (&key module (modulevar (gensym "MODULE-")) lquery access-branch menu-icon menu-tooltip) &body body)
  "Define a new user panel."
  (let* ((name (make-keyword name))
         (category (make-keyword category))
         (categorygens (gensym "ADMINCAT-"))
         (getcategory `(gethash ,category ,categorygens))
         (funcbody (if lquery 
                       `(let ((lquery:*lquery-master-document* NIL))
                          ,(if (and lquery (not (eq lquery T)))
                               `(lquery:$ (initialize ,lquery)))
                          ,@body
                          (concatenate-strings (lquery:$ (serialize :doctype NIL))))
                       `(progn ,@body))))
    `(let* ((,modulevar ,(if module module `(get-module T)))
            (,categorygens (categories (get-module :trivial-profile))))
       (declare (ignorable ,modulevar))
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
       (build-menu))))

(define-user-panel profile user (:menu-icon "icon-user" :menu-tooltip "Profile settings" :lquery (template "trivial-profile/panel-profile.html"))
  ($ "*[data-uibox]" (each #'(lambda (node) (uibox:fill-node node (user)))))
  
  (let* ((parent ($ "#fields li" (parent) (node)))
         (template ($ parent "li" (last) (remove) (node)))
         (fields (db-select T "trivial-profile" (query (:= "user" (user-field (user) "username"))) :limit -1)))
    (db-iterate T "trivial-profile-fields" :all
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

(define-user-panel comments user (:menu-icon "icon-comments" :menu-tooltip "Manage comments on your profile" :lquery (template "trivial-profile/panel-comments.html"))
  (uibox:fill-foreach (model-get T "trivial-profile-comments" (query (:= "user" (user-field (user) "username"))) :sort '(("time" . :DESC)) :limit -1) "tbody tr"))

(defapi profile/edit (displayname email) (:access-branch "user.settings.profile")
  (let ((username (user-field (user) "username")))
    (db-remove T "trivial-profile" (query (:= "user" username)) :limit NIL)
    (if (email-p email)
        (setf (user-field (user) "email") email)
        (error 'api-args-error :apicall 'profile/edit :text "Email-Address is invalid."))
    (if (displayname-p displayname)
        (setf (user-field (user) "displayname") displayname)
        (error 'api-args-error :apicall 'profile/edit :text "Displayname is invalid."))
    (user-save (user))
    (db-iterate T "trivial-profile-fields" :all
                #'(lambda (row)
                    (let ((field (cdr (assoc "field" row :test #'string=))))
                      (db-insert T "trivial-profile" `(("user" . ,username ) ("field" . ,field) ("value" . ,(post-var field))))))))
  (redirect))
