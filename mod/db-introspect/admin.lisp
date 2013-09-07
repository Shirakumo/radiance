#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-db-introspect)

(define-admin-panel database database (:access-branch "admin.database.*" :menu-icon "icon-calendar" :menu-tooltip "Show all collections in the database" :lquery (template "db-introspect/database.html"))
  (uibox:fill-foreach (loop for collection in (db-collections T)
                         collect `(:name ,collection :records ,(length (db-select T collection :all :limit -1)))) "tbody tr")
  )

(define-admin-panel collection database (:access-branch "admin.database.collection.*" :menu-icon "icon-table" :menu-tooltip "View collection contents" :lquery (template "db-introspect/collection.html"))
  (let ((selected (or (post-var "selected")
                      (get-var "name"))))
    (if selected
        (if (string= (post-or-get-var "action") "delete")
            NIL
            (display-collection (if (listp selected) (first selected) selected)))
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
                                        (uibox:fill-node row record)
                                        (return row)))))))
      ($ rows (insert-before template))
      ($ template (remove))
      ($ "input[name=\"name\"]" (val name)))))

(define-admin-panel record database (:access-branch "admin.database.collection.record.*" :menu-icon "icon-list-alt" :menu-tooltip "View record contents" :lquery (template "db-introspect/record.html"))
  (let ((selected (or (post-var "selected")
                      (get-var "id")))
        (name (post-or-get-var "name")))
    (if (and selected name)
        NIL
        (redirect))))
