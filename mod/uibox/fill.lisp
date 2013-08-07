#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-uibox)

(defmacro with-initialized-lquery (template &rest body)
  `(let ((lquery:*lquery-master-document*
          ,(cond
            ((not template)             `lquery:*lquery-master-document*)
            ((stringp template)         `(load-page (pathname ,template)))
            ((typep template 'pathname) `(load-page ,template))
            (T `(cond
                  ((not ,template)                   lquery:*lquery-master-document*)
                  ((or (dom::node-p ,template)
                       (dom::document-p ,template))  ,template)
                  ((stringp ,template)               (load-page (pathname ,template)))
                  ((typep ,template 'pathname)       (load-page ,template))
                 (T (error "Template is not of type NIL, NODE, STRING or PATHNAME.")))))))
     ,@body))

(defun fill-foreach (models selector &key template)
  "Fills the node with data using the provided list of alists, plists, data-models or a list in order of the fields.

Selector is a CSS selector that matches the node to be repeated and filled with data.
Data is filled into any node with the data-uibox attribute.
See uibox:fill-node for more information on how the data is filled into the nodes.

Template can either be a dom-node, a pathname or NIL.
If it is a dom-node, all actions will be performed on this dom-node.
If it is a pathname or a string, lQuery will be initialized with the new document.
If it is NIL, it is expected that lQuery has already been initialized with a document."
  (with-initialized-lquery template
    (if (typep models 'hash-table) (setf models (alexandria:hash-table-values models)))
    (let* ((template ($ selector (node)))
           (nodes (loop for model in models
                     for clone = ($ template (clone) (node))
                     do (fill-node clone model)
                       (loop for node in ($ clone "*[data-uibox]")
                          do (fill-node node model))
                     collect clone)))
      ($ selector (parent) (append nodes))
      ($ template (remove)))
    lquery:*lquery-master-document*))

(defun fill-node (node model)
  "Fills data into the node according to uibox constants. Syntax:
DATA-UIBOX : TARGET:field*
TARGET     : text | html | value | class | style | id | ATTRIBUTE | FOREACH
ATTRIBUTE  : attr-NAME
FOREACH    : foreach-SELECTOR"
  (let ((targets (split-sequence:split-sequence #\space (first ($ node (attr :data-uibox))))))
    (loop for temp in targets
       if (> (length temp) 0)
       do (let* ((temp (split-sequence:split-sequence #\: temp))
                 (target (first temp))
                 (data (getdf model (second temp))))
            (when data
              (string-case:string-case (target)
                ("text" ($ node (text data)))
                ("html" ($ node (html data)))
                ("value" ($ node (val data)))
                ("class" ($ node (add-class data)))
                ("id" ($ node (attr :id data)))
                ("style" (let ((css ($ node (attr :style))))
                           ($ node (attr :style (concatenate 'string css data)))))
                (T (cond 
                     ((and (> (length target) 5)
                           (string= target "attr-" :end1 5))
                      ($ node (attr (make-keyword (subseq target 5)) data)))
                     
                     ((and (> (length target) 8)
                           (string= target "foreach-" :end1 8))
                      (fill-foreach data (subseq target 8) :template node))
                     
                     (T (error "Unknown data target directive: ~a" target)))))))))
  ($ node (remove-attr :data-uibox))
  node)
