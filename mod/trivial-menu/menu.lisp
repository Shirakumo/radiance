#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-menu)

(defvar *menu-node* NIL)

(define-hook (:user :lquery-post-processing) (:documentation "Post processing hook to display the menu.")
  ;; Hack to avoid having to change the owner document
  (let ((*menu-node* (init-menu)))
    (when *menu-node*
      (setf (slot-value *menu-node* 'rune-dom::owner) lquery:*lquery-master-document*)
      ($ "head" (append (lquery:parse-html "<link rel=\"stylesheet\" href=\"/static/css/menu.css\" />")))
      ($ "body" (prepend *menu-node*)))))

(define-hook (:server :init) (:documentation "Server init hook to build the menu from the database.")
  (db:create "trivial-menu" '(("pid" :integer) ("sort" :integer) ("title" :varchar 32) ("tooltip" :text) ("access" :varchar 32) ("link" :text)))
  (init-menu))

(defun init-menu ()
  (let ((lquery:*lquery-master-document*))
    ($ (initialize (template "trivial-menu/menu.html")))
    
    (let ((base-ul ($ "#topmenu>ul" (node)))
          (base-li ($ "#topmenu>ul>li" (remove) (node))))
      (labels ((iterate (pid template)
                 (loop for model in (dm:get "trivial-menu" (db:query (:= "pid" pid)) :sort '(("sort" . :ASC)) :limit -1)
                       if (build-li model template)
                         collect it))
               (build-li (model template)
                 (with-fields (_id title tooltip access link) model
                   (when (or (not access) (= 0 (length access)) (user:check access :user (user:current :authenticate T)))
                     (let ((li (dom:clone-node template T)))
                       ($ li "a" (text title) (attr :href link :title tooltip))
                       ($ li "ul" (append (iterate _id template)))
                       li)))))
        ($ base-ul (append (iterate -1 base-li)))))

    (setf *menu-node* ($ "#topmenu" (node)))))
