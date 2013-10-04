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

(defmacro confirm ((message &key (yes "Yes") (no "No") (type :question) classes (confirm-field (gensym "CONFIRM"))) yes-block no-block)
  "Macro to build an intermittant confirm screen."
  (let ((session-field (gensym "REQUEST"))
        (confirm-field (symbol-name confirm-field)))
    `(let ((*radiance-session* (or (session-field *radiance-session* ',session-field)
                                   *radiance-session*)))
       (cond
         ((string= (get-var ,confirm-field) ,yes)
          ,yes-block)
         ((string= (get-var ,confirm-field) ,no)
          ,no-block)
         (T
          ($ (initialize (template "uibox/confirm.html")))
          ($ "#message" (text ,message))
          ($ ".confirm" (attr :class ,(format NIL "~a ~{~a~^ ~}" (string-downcase type) classes)))
          ($ ".yes" (attr :name ,confirm-field :value ,yes))
          ($ ".no" (attr :name ,confirm-field :value ,no)))))))
