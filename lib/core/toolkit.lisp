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
    (v:warn :radiance.server.status "Config-file or *radiance-config-file* are NIL, automatically choosing path through ASDF.")
    (setf config-file (merge-pathnames "radiance.json" (asdf:system-source-directory :radiance))))

  (v:debug :radiance.server.status "Reloading radiance config file from ~a" config-file)
  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *radiance-config* (json:decode-json file))
    (setf *radiance-config-file* config-file)))

(defun config (setting &optional new-value)
  "Get or set configuration values."
  (when new-value
    (v:debug :radiance.server.status "Setting config ~a to ~a" setting new-value)
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

(declaim (inline concatenate-strings))
(defun concatenate-strings (list &optional (delim ""))
  "Joins a list of strings into one string using format."
  (format nil (format nil "~~{~~A~~^~a~~}" delim) list))

(declaim (inline plist->hash-table))
(defun plist->hash-table (&rest plist)
  "Turns the keyword arguments into a hash table."
  (alexandria:plist-hash-table plist))

(declaim (inline package-symbol))
(defun package-symbol (&optional (package *package*))
  "Retrieves the keyword symbol of the package."
  (make-keyword (package-name package)))

(declaim (inline universal-to-unix-time))
(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(declaim (inline unix-to-universal-time))
(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(declaim (inline get-unix-time))
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
(defgeneric (setf getdf) (value model field))

(defmethod getdf ((model list) field)
  (if (keywordp (first model))
      (getf model (if (stringp field) (make-keyword (string-upcase field)) field))
      (if (listp (first model))
          (cdr (assoc field model :test #'equalp))
          (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defmethod (setf getdf) (value (model list) field)
  (if (keywordp (first model))
      (setf (getf model (if (stringp field) (make-keyword (string-upcase field)) field)) value)
      (if (listp (first model))
          (setf (cdr (assoc field model :test #'equalp)) value)
          (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defmethod getdf ((model hash-table) field)
  (gethash field model))

(defmethod (setf getdf) (value (model hash-table) field)
  (setf (gethash field model) value))

(defmethod getdf ((model standard-object) field)
  (let ((slot (find-symbol (string-upcase field) (symbol-package (class-name (class-of model))))))
    (if slot (slot-value model slot))))

(defmethod getdf ((model asdf:component) field)
  (let ((slot (find-symbol (string-upcase field) :ASDF)))
    (if slot (slot-value model slot))))

(defmethod (setf getdf) (value (model standard-object) field)
  (let ((slot (find-symbol (string-upcase field) (symbol-package (class-name (class-of model))))))
    (if slot (setf (slot-value model slot) value))))

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
  (v:trace :radiance.toolkit "Walking directory ~a" dir)
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

(defun find-any (items sequence &rest args &key from-end (start 0) end key test test-not)
  (declare (ignore from-end start end key test test-not))
  (not (loop for item in items
             never (apply #'find item sequence args))))

(defun lambda-keyword-p (symbol)
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun remove-aux-part (lambda-list)
  (if-let ((position (position '&aux lambda-list)))
    (subseq lambda-list 0 position)
    lambda-list))

(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  (remove-if #'lambda-keyword-p (flatten-lambda-list (remove-aux-part lambda-list))))

(defun extract-macro-lambda-vars (macro-lambda-list)
  (loop with varlist = ()
        for i from 0 below (length macro-lambda-list)
        for arg in macro-lambda-list
        do (when (find arg '(&key &optional &rest &body &aux))
             (return (append varlist (extract-lambda-vars (nthcdr i macro-lambda-list)))))
           (unless (lambda-keyword-p arg)
             (if (listp arg)
                 (appendf varlist (extract-lambda-vars arg))
                 (appendf varlist (list arg))))
        finally (return varlist)))

(defun make-key-extensible (generic-lambda-list)
  (if (or (find '&rest generic-lambda-list)
          (find '&body generic-lambda-list))
      generic-lambda-list
      (if (find '&key generic-lambda-list)
          (append generic-lambda-list '(&allow-other-keys))
          (append generic-lambda-list '(&key &allow-other-keys)))))

(defun macro-lambda-list->generic-list (macro-lambda-list)
  (loop with in-required-args = T
        for arg in macro-lambda-list
        collect (cond
                  ((eql arg '&body)
                   (setf in-required-args NIL)
                   '&rest)
                  ((or (eql arg '&rest) (eql arg '&key) (eql arg '&optional))
                   (setf in-required-args NIL)
                   arg)
                  ((and in-required-args (listp arg))
                   (gensym (format NIL "~{~a~^-~}" (extract-lambda-vars arg))))
                  ((listp arg)
                   (car arg))
                  (T arg))))

(defun intern-list-symbols (list package)
  (loop for element in list
        collect (if (and (symbolp element) (not (keywordp element)) (not (null element)))
                    (intern (string-upcase element) package)
                    element)))

(declaim (inline static))
(defun static (path)
  "Create pathname to static resource."
  (merge-pathnames path (merge-pathnames "data/static/" (pathname (config :root)))))

(declaim (inline template))
(defun template (path)
  "Create pathname to template."
  (merge-pathnames path (merge-pathnames "data/template/" (pathname (config :root)))))

(defun read-data-file (pathspec &key (if-does-not-exist :ERROR))
  "Returns the file contents in string format. Any path is relative to the radiance data directory."
  (v:trace :radiance.server.site "Reading data file: ~a" pathspec)
  (with-open-file (stream (merge-pathnames pathspec (merge-pathnames "data/" (pathname (config :root)))) :if-does-not-exist if-does-not-exist)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(declaim (inline error-page))
(defun error-page (errorcode)
  "Signals a condition that provokes the requested error page."
  (v:debug :radiance.server.request "Erroring out to ~a" errorcode)
  (error 'error-page :code errorcode :text "Requested error page."))
