#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-db-introspect)

(define-admin-panel database database (:access-branch "admin.database.*" :menu-icon "icon-calendar" :menu-tooltip "Show all collections in the database" :lquery (template "db-introspect/database.html"))
  (uibox:fill-foreach (loop for collection in (db-collections T)
                         collect `(:name ,collection :records ,(length (db-select T collection :all :limit -1)))) "tbody tr"))

(define-admin-panel collection database (:access-branch "admin.database.collection.*" :menu-icon "icon-table" :menu-tooltip "View collection contents" :lquery (template "db-introspect/collection.html"))
  (let ((selected (get-var "name")))
    (if selected
        (if (string= (post-or-get-var "action") "Delete")
            (uibox:confirm ((format NIL "Really drop the collection ~a and all its data?" selected))
               (progn
                 (db-drop T selected)
                 (redirect "/database/database"))
               (redirect "/database/database"))
            (display-collection selected))
        (redirect))))

(defun display-collection (name)
  ($ "h2" (text (concatenate 'string "Manage Collection " name)))
  (let ((fields (db-apropos T name)))
    (loop with template = ($ "thead .template" (node))
       for name in fields
       collect ($ template (clone) (node) (text name)) into nodes
       finally (progn ($ nodes (insert-before template))
                      ($ template (remove))))
    (let* ((template ($ "tbody tr" (node)))
           (rows (db-iterate
                  T name :all
                  #'(lambda (record)
                      (loop with row = ($ template (clone) (node))
                         with inner-template = ($ row ".template" (node))
                         for name in fields
                         collect ($ inner-template (clone) (node) (text (cdr (assoc name record :test #'string-equal)))) into nodes
                         finally (progn ($ nodes (insert-before inner-template))
                                        ($ inner-template (remove))
                                        (uibox:fill-all row record)
                                        (return row)))))))
      ($ rows (insert-before template))
      ($ template (remove))
      ($ "input[name=\"name\"]" (val name)))))

(define-admin-panel record database (:access-branch "admin.database.collection.record.*" :menu-icon "icon-list-alt" :menu-tooltip "View record contents" :lquery (template "db-introspect/record.html"))
  (let ((selected (or (post-var "selected[]")
                      (get-var "id")))
        (name (post-or-get-var "name")))
    (if (and selected name)
        (string-case:string-case ((post-or-get-var) "action")
          ("Delete" (uibox:confirm ("Are you sure you want to delete the selected record(s)?")
                      (progn
                        (dolist (id (if (listp selected) selected (list selected)))
                          (db-remove T name (query (:= "_id" id))))
                        (redirect (format NIL "/database/collection?name=~a" name)))
                      (redirect (format NIL "/database/collection?name=~a" name))))
          ("Save" (save-record name selected))
          ("Edit" (display-record name (if (listp selected) (first selected) selected)))
          (T (redirect)))
        (redirect))))

(defun display-record (collection id)
  ($ "h2" (text (concatenate 'string "Edit record " id " of " collection)))
  (let ((model (model-get-one T collection (query (:= "_id" id)))))
    (if model
        (loop with template = ($ ".template" (node))
           for key in (db-apropos T collection)
           for val = (model-field model key)
           for node = ($ template (clone) (node))
           do ($ node "label" (text key))
             ($ node "input" (attr :value val :name key))
             ($ node (insert-before template))
           finally ($ template (remove)))
        (uibox:notice "No such record found!" :type :error))))

(defun save-record (collection id)
  (with-model model (collection (query (:= "_id" id)) :save T)
    (dolist (field (db-apropos T collection))
      (setf (model-field model field) (post-var field)))
    (uibox:notice "Record updated.")))
