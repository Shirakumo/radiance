#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-trivial-menu)

(defvar *menu-node* NIL)

(define-hook (:user :lquery-post-processing) (:description "Post processing hook to display the menu.")
  ;; Hack to avoid having to change the owner document
  (when *menu-node*
    (setf (slot-value *menu-node* 'rune-dom::owner) lquery:*lquery-master-document*)
    ($ "head" (append (lquery:parse-html "<link rel=\"stylesheet\" href=\"/static/css/menu.css\" />")))
    ($ "body" (prepend *menu-node*))))

(define-hook (:server :init) (:description "Server init hook to build the menu from the database.")
  (init-menu))

(defun init-menu ()
  (db:create "trivial-menu" '(("pid" :integer) ("sort" :integer) ("title" :varchar 32) ("tooltip" :text) ("link" :text)))
  ($ (initialize (template "trivial-menu/menu.html")))
  
  (let ((base-ul ($ "#topmenu>ul" (node)))
        (base-li ($ "#topmenu>ul>li" (remove) (node))))
    (labels ((iterate (pid template)
               (mapcar #'(lambda (model) (build-li model template))
                       (dm:get "trivial-menu" (db:query (:= "pid" pid)) :sort '(("sort" . :ASC)) :limit -1)))
             (build-li (model template)
               (let ((li (dom:clone-node template T)))
                 (with-fields (_id title tooltip link) model
                   ($ li "a" (text title) (attr :href link :title title))
                   ($ li "ul" (append (iterate _id template)))
                   li))))
      ($ base-ul (append (iterate -1 base-li)))))

  (setf *menu-node* ($ "#topmenu" (node))))
