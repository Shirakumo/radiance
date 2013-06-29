#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-config-file* NIL "Radiance's main JSON configuration file.")
(defvar *radiance-config*      NIL "Radiance's main static configuration.")

(defun load-config (&optional (config-file *radiance-config-file*))
  "(Re)load the static configuration."
  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *radiance-config* (json:decode-json file))
    (setf *radiance-config-file* config-file)))

(defun config (setting &optional new-value (config-file *radiance-config-file*))
  "Get or set configuration values."
  (when new-value
    (setf (cdr (assoc setting *radiance-config*)) new-value)
    (with-open-file (file config-file :if-exists :SUPERSEDE)
      (json:encode-json *radiance-config* file)))
  (cdr (assoc setting *radiance-config*)))

(defsetf config config) 

(defun concatenate-strings (list &optional (delim ""))
  "Joins a list of strings into one string using format."
  (format nil (concatenate 'string "~{~A~^" delim "~}") list))
