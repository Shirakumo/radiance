#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:db-introspect
  (:nicknames #:org.tymoonnext.db-introspect)
  (:use #:cl #:radiance))
(in-package #:db-introspect)

(defun ccount (collection)
  (db:count collection (db:query :all)))

(admin:define-panel database database (:access '(radiance admin database) :icon "fa-calendar" :tooltip "Show all collections in the database")
  (r-clip:process
   (plump:parse (template "database.ctml"))
   :collections (db:collections)))

(admin:define-panel collection database (:access '(radiance admin database) :icon "fa-table" :tooltip "View collection contents")
  (let ((collection (post/get "collection"))
        (confirm (post/get "confirm"))
        (action (post/get "action")))
    (if (and collection action)
        (cond
          ((and (not confirm) (not (string-equal action "show")))
           (r-clip:process
            (plump:parse (template "confirm.ctml"))
            :collection collection
            :action action))
          ((and confirm)
           (when (string-equal confirm "yes")
             (db:drop collection))
           (redirect (format NIL "/database/database")))
          (T
           (r-clip:process
            (plump:parse (template "collection.ctml"))
            :collection collection
            :fields (cons "_id" (mapcar #'first (db:structure collection)))
            :records (dm:get collection (db:query :all)))))
        (redirect "/database/database"))))

(admin:define-panel record database (:access '(radiance admin database) :icon "fa-list-alt" :tooltip "View record contents")
  (let ((collection (post/get "collection"))
        (confirm (post/get "confirm"))
        (id (post/get "id"))
        (action (post/get "action")))
    (if (and collection action id)
        (cond
          ((and (not confirm) (string-equal action "delete"))
           (r-clip:process
            (plump:parse (template "confirm.ctml"))
            :collection collection
            :action action
            :id id))
          ((and confirm (not (string-equal confirm "yes")))
           (redirect (format NIL "/database/collection?collection=~a&action=show" collection)))
          ((string= action "Save")
           (with-model model (collection (db:query (:= '_id id)))
             (dolist (field (mapcar #'first (db:structure collection)))
               (setf (dm:field model field) (post-var field)))
             (dm:save model))
           (redirect (format NIL "/database/collection?collection=~a&action=show" collection)))
          ((string= action "Delete")
           (db:remove collection (db:query (:= '_id id)))
           (redirect (format NIL "/database/collection?collection=~a&action=show" collection)))
          (T
           (r-clip:process
            (plump:parse (template "record.ctml"))
            :collection collection
            :fields (mapcar #'first (db:structure collection))
            :record (dm:get-one collection (db:query (:= '_id id))))))
        (redirect "/database/database"))))
