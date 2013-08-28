#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-uibox)

(defun input-select (name choices &key selected id classes)
  (let ((node (lquery:parse-html "<select></select>")))
    ($ node (attr :name name))
    (if id ($ node (attr :id id)))
    (if classes ($ node (attr :class (format NIL "~{~a~^ ~}" classes))))
    (loop with template = (first (lquery:parse-html "<option></option>"))
       for option = (dom:clone-node template T)
       for choice in choices
       if (listp choice)
         do ($ option (attr :value (cdr choice)) (text (car choice)))
       else
         do ($ option (attr :value choice) (text choice))
       do (if (or (and (listp choice) (equal selected (cdr choice))) (equal selected choice))
              ($ option (attr :selected "selected")))
         ($ node (append option)))
    node))
        
