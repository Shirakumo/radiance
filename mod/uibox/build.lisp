#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-uibox)

(defun input-select (name choices &key selected id classes (test #'string=))
  "Builds a select input form element.
Each item in the choices list can be either an atom or a cons cell.
If a cons cell is used, the car represents the name and the cdr the value of the option.
Test is the test function to compare choice values against the selected argument."
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
       do (if (or (and (listp choice) (funcall test selected (cdr choice))) (funcall test selected choice))
              ($ option (attr :selected "selected")))
         ($ node (append option)))
    node))
        
(defun notice (message &key (type :ok) classes id (prepend T))
  "Builds a notice element and prepends it to the document.
If prepend is NIL, the notice node is returned instead."
  (let ((node (lquery:parse-html "<div></div>")))
    ($ node (text message))
    ($ node (attr :class (format NIL "notice ~a ~{~a~^ ~}" (string-downcase type) classes)))
    (if id ($ node (attr :id id)))
    (if prepend ($ (prepend node)))
    (first node)))
    
