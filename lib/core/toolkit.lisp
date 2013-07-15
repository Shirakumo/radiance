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
  (when (not config-file)
    (log:warn "Config-file or *radiance-config-file* are NIL, automatically choosing path through ASDF.")
    (setf config-file (merge-pathnames "radiance.json" (asdf:system-source-directory :radiance))))

  (log:info "Reloading radiance config file from ~a" config-file)
  (with-open-file (file config-file :if-does-not-exist :ERROR)
    (setf *radiance-config* (json:decode-json file))
    (setf *radiance-config-file* config-file)))

(defun config (setting &optional new-value (config-file *radiance-config-file*))
  "Get or set configuration values."
  (when new-value
    (setf (cdr (assoc setting *radiance-config*)) new-value)
    (with-open-file (file config-file :if-exists :SUPERSEDE)
      (log:info "Writing radiance config file to ~a" config-file)
      (json:encode-json *radiance-config* file)))
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
  (format nil (concatenate 'string "~{~A~^" delim "~}") list))

(defun make-keyword (name) (values (intern (string-upcase name) "KEYWORD")))

(defun read-data-file (pathspec &key (if-does-not-exist :ERROR))
  "Returns the file contents in string format. Any path is relative to the radiance data directory."
  (with-open-file (stream (merge-pathnames pathspec (merge-pathnames "data/" (pathname (config :root)))) :if-does-not-exist if-does-not-exist)
    (let ((seq (make-array (file-length stream) :element-type 'character :fill-pointer t)))
      (setf (fill-pointer seq) (read-sequence seq stream))
      seq)))

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
