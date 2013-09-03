#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defpage profile #u"user./" (:lquery T)
  (ignore-errors (authenticate T))
  (let* ((username (if (= (length (path *radiance-request*)) 0)
                       (user-field (user) "username")
                       (path *radiance-request*)))
         (user (user-get T username)))
    (if (user-saved-p user)
        (progn
          ($ (initialize (template "trivial-profile/profile.html")))
          (ignore-errors (authenticate T))
          (if (authorized-p "user.comment")
              ($ "#profile-comments-submit *[data-uibox]" (each #'(lambda (node) (uibox:fill-node node (user)))))
              ($ "#profile-comments-submit" (remove)))

          (let* ((parent ($ "#profile-details ul"))
                 (template ($ parent "li" (last) (remove) (node)))
                 (fields (db-select T "trivial-profile" (query (:= "user" username)) :limit -1))
                 (color "null")
                 (background "null"))
            (db-iterate T "trivial-profile-fields" (query (:= "public" "T"))
                        #'(lambda (row)
                            (loop with key = (cdr (assoc "field" row :test #'string=))
                               for vrow in fields
                               for field = (cdr (assoc "field" vrow :test #'string=))
                               for value = (cdr (assoc "value" vrow :test #'string=))
                               if (string-equal field "color") do (setf color value)
                               if (string-equal field "background") do (setf background value)
                               if (and (string= field key)
                                       (> (length value) 0))
                               do (let ((clone ($ template (clone) (node))))
                                    ($ clone ".key" (text field))
                                    ($ clone ".value" (text value))
                                    ($ parent (append clone))))))
            ($ "body" (append (lquery:parse-html (format NIL "<script type=\"text/javascript\">customizeProfile(\"~a\", \"~a\");</script>" color background)))))
          
          (uibox:fill-foreach (user-get-actions user 10 :public T) "#profile-actions ul li")
          (uibox:fill-foreach (model-get T "trivial-profile-comments" (query (:= "user" username)) :limit -1 :sort '(("time" . :DESC))) "#profile-comments ul li")
          ($ "*[data-uibox]" (each #'(lambda (node) (uibox:fill-node node user)))))
        (error-page 404))))
