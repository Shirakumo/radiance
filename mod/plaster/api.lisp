#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(defapi raw (id) (:method T)
  "Returns the raw paste text in text/plain format."
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (progn
          (server:set-content-type "text/plain")
          (dm:field paste "text"))
        (error 'api-error :apicall "raw" :module "plaster" :code 404 :text "No such paste found."))))

(defapi paste (id) (:method :GET)
  "Returns all available information about a paste."
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (progn
          (api-return 200 "Paste information."
                      (plist->hash-table
                       :id (id->hash (dm:field paste "_id"))
                       :pid (id->hash (dm:field paste "pid"))
                       :title (dm:field paste "title")
                       :author (dm:field paste "author")
                       :type (dm:field paste "type")
                       :time (dm:field paste "time")
                       :text (dm:field paste "text"))))
        (error 'api-error :apicall "raw" :module "plaster" :code 404 :text "No such paste found."))))
