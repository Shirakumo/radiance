#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defvar *radiance-config-file* NIL "Radiance's main JSON configuration file.")
(defvar *radiance-config*      NIL "Radiance's main static configuration.")

(define-condition radiance-error (error)
  ((text :initarg :text :initform "Undefined Error")
   (code :initarg :code :initform -1))
  (:report (lambda (c s) (format s "~a: ~a (E~4d)" (class-name (class-of c)) (slot-value c 'text) (slot-value c 'code)))))

(define-condition auth-error (radiance-error) ())

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

(defmacro nappend (var &rest lists)
  `(setf ,var (append ,var ,@lists)))

(defvar *unix-epoch-difference*
  (encode-universal-time 0 0 0 1 1 1970 0))

(defun universal-to-unix-time (universal-time)
  (- universal-time *unix-epoch-difference*))

(defun unix-to-universal-time (unix-time)
  (+ unix-time *unix-epoch-difference*))

(defun get-unix-time ()
  "Returns a unix timestamp."
  (universal-to-unix-time (get-universal-time)))

(defvar *random-string-characters* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789")

(defun make-random-string (&optional (length 16) (chars *random-string-characters*))
  "Generates a random string of alphanumerics."
  (coerce (loop with charlength = (length chars) 
             for i below length collect (aref chars (random charlength)))
          'string))

(defgeneric getdf (model field)
  (:documentation "Attempts to extract the requested field from a variety of different data models."))

(defmethod getdf ((model data-model) field)
  (model-field model field))

(defmethod getdf ((model user) field)
  (user-field model field))

(defmethod getdf ((model session) field)
  (session-field model field))

(defmethod getdf ((model list) field)
  (if (keywordp (first model))
      (getf model (if (stringp field) (make-keyword field) field))
      (if (listp (first model))
          (assoc field model)
          (error "Model is of type LIST, but is neither an ALIST or PLIST."))))

(defun authenticated-p (&optional (session *radiance-session*))
  (and session (session-user session) (user-saved-p (session-user session)) (session-active-p session)))

(defun authorized-p (access-branch &optional (session *radiance-session*))
  (and (authenticated-p session) (user-check (session-user session) access-branch)))

(defvar *default-cookie-expire* (* 60 60 24 356))

(defun set-cookie (name &key (value "") domain (path "/") (expires (+ (get-universal-time) *default-cookie-expire*)) (http-only T) secure (response (response *radiance-request*)))
  "Sets a cookie with radiance defaults and ensures proper return object utilization. If domain is NIL, it sets it for multi-subdomain compatibility."
  (flet ((setc (domain) (hunchentoot:set-cookie name :value value :domain domain :path path :expires expires :http-only http-only :secure secure :reply response)))
    (log:debug "Setting cookie ~a on ~a/~a exp ~a (H~a;S~a) to ~a" name domain path expires http-only secure value)
    (if domain
        (setc domain)
        (setc (format NIL ".~a" (domain *radiance-request*))))))

(defun template (path)
  "Create pathname to template."
  (merge-pathnames path (merge-pathnames "data/template/" (pathname (config :root)))))

(defun read-data-file (pathspec &key (if-does-not-exist :ERROR))
  "Returns the file contents in string format. Any path is relative to the radiance data directory."
  (with-open-file (stream (merge-pathnames pathspec (merge-pathnames "data/" (pathname (config :root)))) :if-does-not-exist if-does-not-exist)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

(defun error-page (errorcode)
  "Returns the contents of the requested error page."
  (read-data-file (format nil "static/html/error/~a.html" errorcode)))

(defun file-size (pathspec)
  "Retrieves the file size in bytes."
   (with-open-file (in pathspec :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun upload-file (post-parameter 
                    &key (directory (merge-pathnames "data/static/upload/" (pathname (config :root))))
                      (filename NIL)
                      (max-file-size (config-tree :upload :max-file-size))
                      (allowed-mimes (config-tree :upload :allowed-mimes))
                      (replace-file (config-tree :upload :replace-files))
                      (max-name-length (config-tree :upload :max-name-length))
                      (use-uuids (config-tree :upload :use-uuids))
                      (append-extension (config-tree :upload :append-extension)))
  "Upload a given file using the post parameter as source.
:directory designates the target for the given file and defaults to /data/static/upload/.
:filename defaults to either a UUID if use-uuids is set to T or the original filename.
:max-file-size specifies an upper limit for the file in bytes unless it is <= 0 or NIL.
:allowed-mimes is a list of strings specifying mime types that can be copied or an asterisk for any type.
:replace-file is a boolean to specify if an existing file may be overwritten.
:max-name-length poses a limit on the file length of the target file, excluding the filename extension.
:use-uuids is only considered if filename is NIL.
:append-extension is a boolean to specify whether the extension of the original filename should be kept.

All of these key values except for filename and directory default to values from the configuration file.
If any of the predicates fail, an assertion error condition is signalled."
  (let ((param (hunchentoot:post-parameter post-parameter (request *radiance-request*))))
    (assert (listp param) (param) "Post parameter does not contain a file!")
    (assert (not (not param)) (param) "Post parameter does not exist!")
    (destructuring-bind (tempfile origname mimetype) param
      (assert (or (not allowed-mimes)
                  (string= allowed-mimes "*") 
                  (find mimetype allowed-mimes :test #'string-equal))
              (allowed-mimes) "Mime type ~a is not allowed!" mimetype)
      (assert (or (not max-file-size)
                  (<= 0 max-file-size)
                  (< (file-size tempfile) (* max-file-size 1024)))
              (max-file-size) "File is larger than ~a" max-file-size)
      (let ((pos (search "." origname :from-end T))
            (target NIL))

        (cond (use-uuids (setf filename (format nil "~a" (uuid:make-v4-uuid))))
              ((not filename) (setf filename (if pos (subseq origname 0 param) origname))))
        (assert (or (not max-name-length)
                    (<= 0 max-name-length)
                    (> (length filename) max-name-length))
                (max-name-length) "File name is too long: ~a" (length filename))
        (if (and pos append-extension) (setf filename (concatenate 'string filename (subseq (second param) pos))))
        (setf target (merge-pathnames filename directory))

        (assert (or replace-file (not (file-exists-p target))) (target) "File already exists: ~a" target)
        (copy-file tempfile target :overwrite replace-file)
        (assert (file-exists-p target) (target) "File copy from ~a to ~a failed!" tempfile target)
        (log:info "Copied file ~a from ~a to ~a" origname tempfile target)
        target))))

(defmacro with-uploaded-file ((file post-parameter
                                    &key (open-file NIL)
                                    (directory (merge-pathnames "data/static/upload/" (pathname (config :root))))
                                    (filename NIL)
                                    (max-file-size (config-tree :upload :max-file-size))
                                    (allowed-mimes (config-tree :upload :allowed-mimes))
                                    (replace-file (config-tree :upload :replace-files))
                                    (max-name-length (config-tree :upload :max-name-length))
                                    (use-uuids (config-tree :upload :use-uuids))
                                    (append-extension (config-tree :upload :append-extension)))
                                    &rest body)
  "Uploads a file and binds either the path to it or a stream to the file to the specified variable, depending on :open-file.
See upload-file for more information."
  (let ((filepathvar (gensym "RADIANCE-FILE")))
    `(let* ((,filepathvar (upload-file ,post-parameter :directory ,directory :filename ,filename :max-file-size ,max-file-size :allowed-mimes ,allowed-mimes
                                      :replace-file ,replace-file :max-name-length ,max-name-length :use-uids ,use-uuids :append-extension ,append-extension))
            (,file ,(if open-file `(open ,filepathvar) filepathvar)))
       ,@body
       
       ,(if open-file `(close ,filepathvar)))))
