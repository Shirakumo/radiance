#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

(defvar *current-db* NIL)
(defvar *current-con* NIL)

(defun db:connect (database-name)
  (with-simple-restart (skip "Skip connecting.")
    (flet ((err (msg) (error 'database-connection-failed :database database-name :message msg)))
      (let ((conn (config-tree :sqlite :connections database-name)))
        (unless conn (err "No such connection found."))
        (when *current-con*
          (warn 'database-connection-already-open :database database-name)
          (db:disconnect))
        ;; Spec restarts for already open.
        (l:info :database "Connecting ~a ~a" database-name conn)
        (setf *current-db* database-name
              *current-con* (sqlite:connect (if (string= conn ":memory:")
                                                conn
                                                (uiop:parse-native-namestring conn))))
        (trigger 'db:connected)))))

(defun db:disconnect ()
  (l:info :database "Disconnecting ~a" *current-db*)
  (sqlite:disconnect *current-con*)
  (setf *current-con* NIL
        *current-db* NIL)
  (trigger 'db:disconnected))

(defun db:connected-p ()
  (not (null *current-con*)))
