#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-menu)

(defgeneric show-menu (module))
(defgeneric init-menu (module))


(defmethod init-menu ((module trivial-menu))
  (db-create T "trivial-menu" '(("pid" :integer) ("sort" :integer) ("title" :varchar 32) ("tooltip" :text) ("link" :text)))
  ($ (initialize (template "trivial-menu/menu.html")))
  
  (let ((base-ul ($ "#topmenu>ul" (node)))
        (base-li ($ "#topmenu>ul>li" (remove) (node))))
    (labels ((iterate (pid template)
               (mapcar #'(lambda (model) (build-li model template))
                       (model-get T "trivial-menu" (query (:= "pid" pid)) :sort '(("sort" . :ASC)) :limit -1)))
             (build-li (model template)
               (let ((li (dom:clone-node template T)))
                 (with-fields (_id title tooltip link) model
                   ($ li "a" (text title) (attr :href link :title title))
                   ($ li "ul" (append (iterate _id template)))
                   li))))
      ($ base-ul (append (iterate -1 base-li)))))

  (setf (menu-node module) ($ "#topmenu" (node))))

(defmethod show-menu ((module trivial-menu))
  ;; Hack to avoid having to change the owner document
  (when (menu-node module)
    (setf (slot-value (menu-node module) 'rune-dom::owner) lquery:*lquery-master-document*)
    ($ "head" (append (lquery:parse-html "<link rel=\"stylesheet\" href=\"/static/css/menu.css\" />")))
    ($ "body" (prepend (menu-node module)))))

(defhook :user :lquery-post-processing (get-module :trivial-menu) #'show-menu)
(defhook :server :init (get-module :trivial-menu) #'init-menu)

