#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defun authenticated-p (&optional (session *radiance-session*))
  "Returns T if the current user is using an authenticated session."
  (and session (session-user session) (user-saved-p (session-user session)) (session-active-p session)))

(defun authorized-p (access-branch &optional (session *radiance-session*))
  "Returns T if the current user is authorized to the given access branch."
  (and (authenticated-p session) (user-check (session-user session) access-branch)))

(defun user (&key default authenticate)
  "Returns the currently logged in user or default."
  (when authenticate
    (setf *radiance-session* (authenticate T)))
  (or (and *radiance-session* (session-user *radiance-session*)) default))

(defun set-cookie (name &key (value "") domain (path "/") (expires (+ (get-universal-time) *default-cookie-expire*)) (http-only T) secure (reply *radiance-reply*))
  "Sets a cookie with defaults and ensures proper return object utilization. If domain is NIL, it sets it for multi-subdomain compatibility."
  (flet ((setc (domain) (hunchentoot:set-cookie name :value value :domain domain :path path :expires expires :http-only http-only :secure secure :reply reply)))
    (v:debug :radiance.server.request "Setting cookie '~a' on ~a ~a exp ~a (HTTP ~a;SECURE ~a) to ~a" name domain path expires http-only secure value)
    (if domain
        (setc domain)
        (setc (format NIL ".~a" (domain *radiance-request*))))))

(defun get-var (name &optional (request *radiance-request*))
  "Returns a GET variable. If the name ends with [], it assumed to be an array and a list of all values is returned."
  (declare (optimize (speed 3)) (string name))
  (if (and (> (length name) 2) (string= name "[]" :start1 (- (length name) 2)))
      (assoc-all name (get-vars) :val #'cdr :test #'string=)
      (hunchentoot:get-parameter name request)))

(defun get-vars (&optional (request *radiance-request*))
  "Returns an alist of all GET variables."
  (hunchentoot:get-parameters* request))

(defun post-var (name &optional (request *radiance-request*))
  "Returns a POST variable. If the name ends with [], it assumed to be an array and a list of all values is returned."
  (declare (optimize (speed 3)) (string name))
  (if (and (> (length name) 2) (string= name "[]" :start1 (- (length name) 2)))
      (assoc-all name (post-vars) :val #'cdr :test #'string=)
      (hunchentoot:post-parameter name request)))

(defun post-vars (&optional (request *radiance-request*))
  "Returns an alist of all POST variables."
  (hunchentoot:post-parameters* request))

(defun post-or-get-var (name &optional (request *radiance-request*))
  "Returns a POST variable or, if not provided, the GET variable of the same name."
  (or (post-var name request) (get-var name request)))

(defun cookie-var (name &optional (request *radiance-request*))
  "Returns a COOKIE variable."
  (hunchentoot:cookie-in name request))

(defsetf cookie-var (cookie) (value)
  `(set-cookie ,cookie :value ,value))

(defun cookie-vars (&optional (request *radiance-request*))
  "Returns an alist of all COOKIE variables."
  (hunchentoot:cookies-in* request))

(defun header-var (name &optional (request *radiance-request*))
  "Returns a HEADER variable."
  (hunchentoot:header-in name request))

(defun header-vars (&optional (request *radiance-request*))
  "Returns an alist of all HEADER variables."
  (hunchentoot:headers-in* request))

(defmacro with-var-func (fun (&rest vars) &body body)
  "Constructs a basic with-X let."
  `(let (,@(loop for var in vars
              for varname = (if (listp var) (first var) var)
              for funcname = (if (listp var) (second var) (string-downcase (symbol-name var)))
              collect `(,varname (funcall ,fun ,funcname))))
     ,@body))

(defmacro with-get ((&rest vars) &body body)
  "Same as with-slots, but for GET variables.
Uses *radiance-request* to retrieve variables.
Note that changes to the variables will not be saved
in the actual request and are therefore purely temporary."
  `(with-var-func #'get-var (,@vars) ,@body))

(defmacro with-post ((&rest vars) &body body)
  "Same as with-slots, but for POST variables.
Uses *radiance-request* to retrieve variables.
Note that changes to the variables will not be saved
in the actual request and are therefore purely temporary."
  `(with-var-func #'post-var (,@vars) ,@body))

(defmacro with-post-or-get ((&rest vars) &body body)
  "Same as with-slots, but for POST and GET variables.
Uses *radiance-request* to retrieve variables.
Note that changes to the variables will not be saved
in the actual request and are therefore purely temporary."
  `(with-var-func #'post-or-get-var (,@vars) ,@body))

(defmacro with-header ((&rest vars) &body body)
  "Same as with-slots, but for HEADER variables.
Uses *radiance-request* to retrieve variables.
Note that changes to the variables will not be saved
in the actual request and are therefore purely temporary."
  `(with-var-func #'header-var (,@vars) ,@body))

(defmacro with-cookie ((&rest cookies) &body body)
  "Same as with-slots, but for COOKIE variables.
Uses *radiance-request* to retrieve variables.
Changes to these cookies will be sent along to the browser with default cookie settings."
  `(symbol-macrolet
       ,(loop for cookie in cookies 
           for varname = (if (listp cookie) (first cookie) cookie)
           for fieldname = (if (listp cookie) (second cookie) (string-downcase (symbol-name cookie)))
           collect `(,varname (cookie-var ,fieldname)))
     ,@body))

(defun request-method (&optional (request *radiance-request*))
  "Returns the http-request method."
  (hunchentoot:request-method request))

(defun remote-address (&optional (request *radiance-request*))
  "Returns the remote address of the request."
  (hunchentoot:remote-addr* request))

(defun set-content-type (content-type &optional (reply *radiance-reply*))
  "Change the content-type of the current page."
  (v:debug :radiance.server.request "Setting content-type to: ~a" content-type)
  (setf (hunchentoot:content-type* reply) content-type))

(defun redirect (&optional (uri-or-string (get-redirect)))
  "Redirects to the requested URI."
  (v:debug :radiance.server.request "Redirecting to ~a" uri-or-string)
  (hunchentoot:redirect 
   (if (stringp uri-or-string)
       uri-or-string
       (uri->url uri-or-string))))

(defun get-redirect (&optional (default "/") (request *radiance-request*))
  (or (hunchentoot:get-parameter "redirect" request)
      (hunchentoot:post-parameter "redirect" request)
      (if *radiance-session* (session-field *radiance-session* "redirect"))
      (hunchentoot:referer request)
      default))

(defun static (path)
  "Create pathname to static resource."
  (merge-pathnames path (merge-pathnames "data/static/" (pathname (config :root)))))

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

(defun error-page (errorcode)
  "Signals a condition that provokes the requested error page."
  (v:debug :radiance.server.request "Erroring out to ~a" errorcode)
  (error 'error-page :code errorcode :text "Requested error page."))

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
  (v:debug :radiance.server.request "Attemptint to upload file from ~a" post-parameter)
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
        (v:debug :radiance.server.request "Copied file ~a from ~a to ~a" origname tempfile target)
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
                                    &body body)
  "Uploads a file and binds either the path to it or a stream to the file to the specified variable, depending on :open-file.
See upload-file for more information."
  (let ((filepathvar (gensym "RADIANCE-FILE")))
    `(let* ((,filepathvar (upload-file ,post-parameter :directory ,directory :filename ,filename :max-file-size ,max-file-size :allowed-mimes ,allowed-mimes
                                      :replace-file ,replace-file :max-name-length ,max-name-length :use-uids ,use-uuids :append-extension ,append-extension))
            (,file ,(if open-file `(open ,filepathvar) filepathvar)))
       ,@body
       
       ,(if open-file `(close ,filepathvar)))))

(defmacro defpage (name uri (&key module (modulevar (gensym "MODULE")) access-branch lquery) &body body)
  "Defines a new page for the given module that will be available on the
specified URI. If access-branch is given, an authorization check on the
current session at page load will be performed. If lquery is non-NIL,
lQuery will be initialized with the given pathspec and the page output
will be set to the lQuery serialization, unless the response field of
the *radiance-request* is already set. If lQuery is unset, the return
value of the request is automatically chosen."
  (let ((name (intern (format nil "PAGE-~a" name)))
        (urigens (gensym "URI")) (modgens (gensym "MODULE"))
        (description (format nil "Page call for ~a" module))
        (funcbody (if lquery 
                      `(progn 
                         ,(if (and lquery (not (eq lquery T)))
                              `(lquery:$ (initialize ,lquery)))
                         ,@body
                         (unless (response *radiance-request*)
                           (trigger :user :lquery-post-processing)
                           (concatenate-strings (lquery:$ (serialize)))))
                      `(progn ,@body))))
    `(let ((,urigens ,uri)
           (,modgens ,(if module module `(get-module T))))
       (v:debug :radiance.server.site "Defining new site ~a on ~a for ~a" ',name ,urigens ,modgens)
       (defmethod ,name ((,modulevar (eql ,modgens)))
         (declare (ignorable ,modulevar))
         (v:trace :radiance.server.request "Entering method for ~a page" ',name)
         ,(if access-branch
              `(progn
                 (ignore-errors (authenticate T))
                 (if (authorized-p ,access-branch)
                     ,funcbody
                     (error-page 403)))
              funcbody))
       (defhook :page ',name ,modgens #',name 
                :description ,description
                :fields (acons :uri ,urigens ()))
       (register T ',name (module-symbol ,modgens) ,urigens))))

(defmacro define-file-link (name uri pathspec &key access-branch module content-type)
  "Defines a link of a given URI to a file. Useful for things like
favicon.ico, robots.txt, humans.txt or other files that cannot be in
the static/ directory."
  `(defpage ,name ,uri (:module ,module :access-branch ,access-branch)
    (hunchentoot:handle-static-file ,pathspec ,content-type)))

(defun link (name &key (module (get-module T)) (type :URI))
  "Returns the link to the requested page or API function. Type can
be one of the following values: :URI :function :hook."
  (let ((name (intern (format nil "PAGE-~a" name))))
    (loop for hook in (get-hooks :page name)
       if (eq module (module hook))
       return (case type
                (:URI (hook-field hook :uri))
                (:function (hook-function hook))
                (:hook hook)))))

(defmacro defapi (name (&rest args) (&key (method T) (module (get-module T)) (modulevar (gensym "MODULE")) access-branch) &body body)
  "Defines a new API function for the given module. The arguments specify
REST values that are expected (or not according to definition) on the
API call. Any variable can have a default value specified. If 
access-branch is given, an authorization check on the current session
at page load will be performed. Method can be one of T :GET :POST
:PUT :PATCH :DELETE. The API call will only be performed if the
http-method of the request matches, or always if the requested method
type is set to be T. The return value of the body should be an
alist or an URI. This will automatically be transformed into the
requested output type or a page redirect in the case of an URI."
  (assert (not (eql module NIL)) () "Module cannot be NIL! (Are you in-module context?)")
  (assert (find method '(T :GET :POST :PUT :PATCH :DELETE)) () "Method has to be one of T :GET :POST :PUT :PATCH :DELETE")
  (let ((fullname (intern (format nil "API-~a" name)))
        (name (make-keyword name))
        (funcbody `(progn ,@body))
        (modgens (gensym "MODULE-"))
        (methodgens (gensym "METHOD-")))
    `(let ((,modgens (get-module ,(module-symbol module))))
       (v:debug :radiance.server.site "Defining API page ~a for ~a" ',fullname ,modgens)
       (defmethod ,fullname ((,modulevar (eql ,modgens)) ,methodgens)
         (declare (ignorable ,modulevar ,methodgens))
         (v:trace :radiance.server.request "Entering method for ~a page" ',fullname)
         ,(unless (eq method T)
            `(unless (eq ,methodgens ,method)
               (call-next-method)
               (return-from ,fullname)))
         (,(case method
             (:POST 'with-post)
             (:GET 'with-get)
             (otherwise 'with-post-or-get))
                (,@(mapcar #'(lambda (x) (if (listp x) (car x) x)) args))
           ,@(loop for arg in args
                if (not (listp arg))
                collect `(assert (not (null ,arg)) () 'api-args-error :module ,modulevar :apicall ',name :text (format NIL "Argument ~a required." ',arg)))
           ,(if access-branch
                `(progn 
                   (ignore-errors (authenticate T)) 
                   (if (authorized-p ,access-branch)
                       ,funcbody
                       (error 'api-auth-error :module ,modulevar :apicall ',name :text "Not authorized.")))
                (progn funcbody))))
       (defhook :api ',name ,modgens #',fullname
                :description ,(format nil "API call for ~a" module)))))

(defun api-return (code text &optional data)
  "Generates an API response in the proper format:
  (:CODE code :TEXT text :DATA data)"
  (plist->hash-table :CODE code :TEXT text :DATA data))

(defun api-format (format data)
  "Turn a plist into the requested format."
  (let ((format (gethash format *radiance-api-formats*)))
    (if format
        (progn
          (setf (hunchentoot:content-type* *radiance-reply*) (second format))
          (funcall (third format) data))
        (plist->format :none NIL))))

(defmacro define-api-format (name content-type datavar &body body)
  "Define a new API output format function."
  (let ((name (make-keyword name)))
    `(progn
       (v:debug :radiance.server.site "Defining new api format ~a, ~a" ,name ,content-type)
       (setf (gethash ,name *radiance-api-formats*)
             (list ,name ,content-type
                   (lambda (,datavar) ,@body))))))
             
(define-api-format none "text/plain; charset=utf-8" data
  (declare (ignore data))
  "Unknown format.")
