#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defapi user/search (query) ()
  (api-return 200 "Matching usernames"
              (db-iterate T "verify-users" (query (:matches "username" query))
                          #'(lambda (data) (cdr (assoc "username" data :test #'string-equal))))))
