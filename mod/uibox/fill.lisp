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
    (let* ((parent ($ selector (node) (parent) (node)))
           (nodes (loop with template = ($ selector (node) (remove) (node)) 
                     for model in models
                     for clone = ($ template (clone) (node))
                     do (fill-all clone model)
                     collect clone)))
      ($ parent (prepend nodes)))
    lquery:*lquery-master-document*))

(defun fill-all (root-node model)
  "Fills all nodes within the root node with the data from the specified model.
See fill-node for more information."
  (loop for node in ($ root-node "*[data-uibox]")
     do (fill-node node model)))

(defun parse-targets (string node)
  (let ((length (length string)))
    (when (> length 0)
      (loop 
         for previous = 0 then (1+ pointer)
         for pointer = (or (search " " string) length) then (if (< pointer length) (search " " string :start2 (1+ pointer)) length)
         while (< previous length)
         for colonpos = (search ":" string :start2 previous :end2 pointer)
         if colonpos
         collect (multiple-value-bind (read length) (read-from-string (subseq string (1+ colonpos)))
                   (setf pointer (+ colonpos length))
                   (cons (subseq string previous colonpos) read)) into tokens
         else
         collect (cons (subseq string previous pointer)
                       (read-from-string
                        (string-case:string-case (string)
                          ("text" ($ node (text) (node)))
                          ("html" ($ node (html) (node)))
                          ("value" ($ node (attr :value) (node)))
                          ("class" ($ node (attr :class) (node)))
                          ("id" ($ node (attr :id) (node)))
                          ("style" ($ node (attr :style) (node)))
                          (T (if (and (> (length string) 5)
                                      (string= string "attr-" :end1 5))
                                 ($ node (attr (make-keyword (string-upcase (subseq string 5)))) (node))))))) into tokens
         finally (return tokens)))))

(defun parse-data (read model)
  (etypecase read
    (symbol (getdf model (string-downcase read)))
    (string read)
    (list (parse-data-function (make-keyword (string-upcase (car read))) (cdr read) model))
    (uri (uri->url read))))

(defgeneric parse-data-function (function args model))

(defmacro define-fill-function (name (modelname &rest args) &body body)
  (let ((argsgen (gensym "ARGS")))
    `(defmethod parse-data-function ((func (eql ,(make-keyword (string-upcase name)))) ,argsgen ,modelname)
       (destructuring-bind (,@args) ,argsgen
         ,@body))))

(define-fill-function concat (model &rest args)
  (format NIL "~{~a~}" (mapcar #'(lambda (arg) (parse-data arg model)) args)))

(define-fill-function make-uri (model urldesc)
  (uri->context-url (make-uri (concatenate 'string "/" (parse-data urldesc model)))))

(define-fill-function avatar (model &optional (size 128) (user model))
  (if (not (eq model user)) (setf user (parse-data user model)))
  (if (stringp user) (setf user (user-get T user)))
  (profile-avatar T user size))

(define-fill-function name (model &optional (user model))
  (if (not (eq model user)) (setf user (parse-data user model)))
  (if (stringp user) (setf user (user-get T user)))
  (user-field user "displayname"))

(define-fill-function date (model field &rest format)
  (if format
      (timestamp-to-date (parse-data field model) format)
      (timestamp-to-date (parse-data field model))))

(define-fill-function datetime (model field)
  (timestamp-to-datetime (parse-data field model)))

(define-fill-function parse (model field)
  (parse T (parse-data field model)))

(defun fill-node (node model)
  "Fills data into the node according to uibox constants. Syntax:
DATA-UIBOX : TARGET:field*
TARGET     : text | html | value | class | style | id | ATTRIBUTE | FOREACH
ATTRIBUTE  : attr-NAME
FOREACH    : foreach-SELECTOR"
  (let ((targets (parse-targets (first ($ node (attr :data-uibox))) node)))
    (loop for temp in targets
       do (let ((target (car temp))
                (data (parse-data (cdr temp) model)))
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
                      ($ node (attr (make-keyword (string-upcase (subseq target 5))) data)))
                     
                     ((and (> (length target) 8)
                           (string= target "foreach-" :end1 8))
                      (fill-foreach data (subseq target 8) :template node))
                     
                     (T (error "Unknown data target directive: ~a" target)))))))))
  ($ node (remove-attr :data-uibox))
  node)
