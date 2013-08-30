#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-profile)

(defpage profile #u"user./" (:lquery T)
  (let ((user (user-get T (path *radiance-request*))))
    (if (user-saved-p user)
        (progn
          ($ (initialize (template "trivial-profile/profile.html")))
          (ignore-errors (authenticate T))
          (if (authorized-p "user.comment")
              ($ "#profile-comments-submit *[data-uibox]" (each #'(lambda (node) (uibox:fill-node node (user)))))
              ($ "#profile-comments-submit" (remove)))
          
          (uibox:fill-foreach (user-get-actions user 10 :public T) "#profile-actions ul li")
          (uibox:fill-foreach (model-get T "trivial-profile-comments" (query (:= "user" (path *radiance-request*))) :limit -1 :sort '(("time" . :DESC))) "#profile-comments ul li")
          ($ "*[data-uibox]" (each #'(lambda (node) (uibox:fill-node node user)))))
        (error-page 404))))
