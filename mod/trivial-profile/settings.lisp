#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defvar *menu* ())
(defvar *categories* (make-hash-table))

(defpage user-settings #u"user./settings" (:lquery (template "trivial-profile/settings.html") :access-branch "user.settings.*")
  (uibox:fill-foreach *menu* "#template")
  ($ "#my-profile" (attr :href (concatenate 'string "/" (user-field (user) "username"))))
  (let ((pathparts (cdr (split-sequence:split-sequence #\/ (string-downcase (path *radiance-request*))))))
    (if (< (length pathparts) 2) (setf pathparts (list "user" "profile")))
    ($ (find (format NIL "a[href=\"/settings/~a/~a\"]" (first pathparts) (second pathparts))) (parent) (add-class "active"))
    
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
     for link = (string-downcase (format NIL "/settings/~a/~a" module panel))
     collect (list :panel (string-upcase (string-downcase panel) :end 1) :link link :icon (or icon "") :title (or tooltip ""))))

(define-interface-method profile:define-panel (name category options &body body)
  (destructuring-bind (&key lquery access-branch menu-icon menu-tooltip) options
    (let* ((name (make-keyword name))
           (category (make-keyword category))
           (getcategory `(gethash ,category *categories*))
           (funcbody (if lquery 
                         `(let ((lquery:*lquery-master-document* NIL))
                            ,(if (and lquery (not (eq lquery T)))
                                 `(lquery:$ (initialize ,lquery)))
                            ,@body
                            (concatenate-strings (lquery:$ (serialize :doctype NIL))))
                         `(progn ,@body))))
      `(progn
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
  (when (or (eql (profile:implementation) :trivial-profile)
            (not (profile:implementation)))
    (profile:implementation :trivial-profile)))

(defapi profile/edit (displayname email) (:access-branch "user.settings.profile")
  (let ((username (user:field (user) "username")))
    (db:remove "trivial-profile" (db:query (:= "user" username)) :limit NIL)
    (if (email-p email)
        (setf (user:field (user) "email") email)
        (error 'api-args-error :apicall 'profile/edit :text "Email-Address is invalid."))
    (if (displayname-p displayname)
        (setf (user:field (user) "displayname") displayname)
        (error 'api-args-error :apicall 'profile/edit :text "Displayname is invalid."))
    (user:save (user))
    (db:iterate "trivial-profile-fields" :all
                #'(lambda (row)
                    (let ((field (cdr (assoc "field" row :test #'string=))))
                      (db:insert "trivial-profile" `(("user" . ,username ) ("field" . ,field) ("value" . ,(server:post field))))))))
  (server:redirect (get-redirect)))
