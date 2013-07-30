#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defun load-config (&optional (config-file *radiance-config-file*))
  "(Re)load the static configuration."
  (when (not config-file)
    (log:warn "Config-file or *radiance-config-file* are NIL, automatically choosing path through ASDF.")
    (setf config-file (merge-pathnames "radiance.json" (asdf:system-source-directory :radiance))))

  (log:info "Reloading radiance config file from ~a" config-file)
  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *radiance-config* (json:decode-json file))
    (setf *radiance-config-file* config-file)))

(defun config (setting &optional new-value)
  "Get or set configuration values."
  (when new-value
    (setf (cdr (assoc setting *radiance-config*)) new-value))
  (cdr (assoc setting *radiance-config*)))

(defun config-tree (&rest branches)
  "Retrieve a configuration value based on a branch."
  (let ((value *radiance-config*))
    (loop for branch in branches
       do (setf value (cdr (assoc branch value))))
    value))

(defsetf config config)

(defun concatenate-strings (list &optional (delim ""))
  "Joins a list of strings into one string using format."
  (format nil (format nil "~~{~~A~~^~a~~}" delim) list))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun package-symbol (&optional (package *package*))
  "Retrieves the keyword symbol of the package."
  (make-keyword (package-name package)))

(defmacro nappend (var &rest lists)
  `(setf ,var (append ,var ,@lists)))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Returns a unix timestamp."
  (universal-to-unix-time (get-universal-time)))

(defun make-random-string (&optional (length 16) (chars *random-string-characters*))
  "Generates a random string of alphanumerics."
  (coerce (loop with charlength = (length chars) 
             for i below length collect (aref chars (random charlength)))
          'string))

(defgeneric getdf (model field)
  (:documentation "Attempts to extract the requested field from a variety of different data models."))

(defmethod getdf ((model list) field)
  (if (keywordp (first model))
      (getf model (if (stringp field) (make-keyword field) field))
      (if (listp (first model))
          (assoc field model)
          (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defun file-size (pathspec)
  "Retrieves the file size in bytes."
   (with-open-file (in pathspec :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun assoc-all (item alist &key (key #'car) (test #'eql) (test-not (constantly NIL)) (val #'identity))
  "Returns all values in the alist that match the predicate.
By default, the values are the cons cells themselves and the car of each cons is matched with EQL."
  (loop for cons in alist
     for keyval = (funcall key cons)
     if (and (funcall test item keyval) (not (funcall test-not item keyval)))
     collect (funcall val cons)))
