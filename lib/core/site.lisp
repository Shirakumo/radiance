#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)


(defun authenticated-p (&optional (session *radiance-session*))
  (and session (session-user session) (user-saved-p (session-user session)) (session-active-p session)))

(defun authorized-p (access-branch &optional (session *radiance-session*))
  (and (authenticated-p session) (user-check (session-user session) access-branch)))

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

