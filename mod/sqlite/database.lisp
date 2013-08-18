#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sqlite)

(implement 'database (get-module 'sqlite))

(defmethod db-connect ((db sqlite) dbname &key (root-path (merge-pathnames "data/" (pathname (config :root)))))
  (log:info "Connecting to SQLite database ~a on ~a" dbname root-path)
  (setf (dbinstance db)
        (sqlite:connect (merge-pathnames dbname root-path))))

(defmethod db-disconnect ((db sqlite) &key)
  (log:info "Disconnecting...")
  (sqlite:disconnect (dbinstance db))
  (setf (dbinstance db) NIL))

(defmethod db-connected-p ((db sqlite) &key)
  (not (null (dbinstance db))))

(defmethod db-collections ((db sqlite) &key)
  )

(defmethod db-create ((db sqlite) (collection string) fields &key indices)
  )
  
(defmethod db-select ((db sqlite) (collection string) query &key (skip 0) (limit 0) sort) 
  )

(defmethod db-iterate ((db sqlite) (collection string) query function &key (skip 0) (limit 0) sort)
  )

(defmethod db-insert ((db sqlite) (collection string) data) 
  )

(defmethod db-remove ((db sqlite) (collection string) query &key (skip 0) (limit 0)) 
  )

(defmethod db-update ((db sqlite) (collection string) query data &key (skip 0) (limit 0) replace) 
  )

(defmethod db-apropos ((db sqlite) (collection string))
  )
