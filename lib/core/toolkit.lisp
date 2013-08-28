#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmacro time-spent (&body body)
  "Returns three values: the return of the last body form, the real time passed and the run time passed."
  (let ((real1 (gensym)) (real2 (gensym)) (run1 (gensym)) (run2 (gensym)) (result (gensym)))
    `(let* ((,real1 (get-internal-real-time))
            (,run1 (get-internal-run-time))
            (,result (progn ,@body))
            (,real2 (get-internal-real-time))
            (,run2 (get-internal-run-time)))
       (declare (fixnum ,real1 ,run1 ,real2 ,run2))
       (values ,result 
               (/ (- ,real2 ,real1) internal-time-units-per-second)
               (/ (- ,run2 ,run1) internal-time-units-per-second)))))

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

(defmacro config-tree (&rest branches)
  "Retrieve a configuration value based on a branch."
  (labels ((rec (branches)
             (if branches
                 `(cdr (assoc ,(car branches) ,(rec (cdr branches))))
                 `*radiance-config*)))
    (rec (reverse branches))))

(defsetf config config)

(defun concatenate-strings (list &optional (delim ""))
  "Joins a list of strings into one string using format."
  (format nil (format nil "~~{~~A~~^~a~~}" delim) list))

(defun plist->hash-table (&rest plist)
  "Turns the keyword arguments into a hash table."
  (alexandria:plist-hash-table plist))

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

(defmethod getdf ((model hash-table) field)
  (gethash field model))

(defmethod getdf ((model standard-object) field)
  (let ((slot (find-symbol (string-upcase field) (symbol-package (class-name (class-of model))))))
    (if slot (slot-value model slot))))

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

(defun walk-directory (dir fn &key directories (files T) (recursive T) (test (constantly T)) (rec-test (constantly T)) (follow-symlinks T) (first NIL) (if-does-not-exist :error))
  "Iterates over files and directories in dir and applies fn.

If directories is T, directories are tested and eventually passed to fn.
If files is T, files are tested and eventually passed to fn.
If recursive is T, each directory that passes rec-test is also scanned.
If follow-symlinks is T, symlinks are resolved and may follow outside of the original directory.
First can be one of NIL, :FILES or :DIRECTORIES and decides which type is tested/passed first, this also affects recursion order.
If if-does-not-exist is :error, an error is thrown in case the directory cannot be found."
  (labels ((consider (file)
             (if (cl-fad:directory-pathname-p file)
                 (progn 
                   (if (and directories (funcall test file))
                       (funcall fn file))
                   (if (and recursive (funcall rec-test file))
                       (walk file)))
                 (if (and files (funcall test file))
                     (funcall fn file))))
           (walk (directory)
             (loop with delegated = ()
                for file in (cl-fad:list-directory directory :follow-symlinks follow-symlinks)
                if (cl-fad:directory-pathname-p file)
                  do (if (eq first :FILES)
                         (push file delegated)
                         (consider file))
                else 
                  do (if (eq first :DIRECTORIES)
                         (push file delegated)
                         (consider file))
                finally (mapc #'consider delegated))))
    (let ((dir (cl-fad:pathname-as-directory dir)))
      (cond ((cl-fad:directory-exists-p dir)
             (walk dir))
            ((eq if-does-not-exist :error)
             (error "File ~S does not exist." dir))))))
