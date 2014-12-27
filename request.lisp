#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *request*)
(defvar *response*)
(defvar *session*)
(defvar *default-external-format* :UTF-8)
(defvar *default-content-type* "text/html")

(defun *request* () *request*)
(defun *response* () *response*)
(defun *session* () *session*)

(defclass request ()
  ((uri :initarg :uri :initform (error "URI required.") :accessor uri)
   (http-method :initarg :http-method :initform :GET :accessor http-method)
   (headers :initarg :headers :initform (make-hash-table :test 'equalp) :accessor headers)
   (post-data :initarg :post-data :initform (make-hash-table :test 'equalp) :accessor post-data)
   (get-data :initarg :get-data :initform (make-hash-table :test 'equalp) :accessor get-data)
   (cookies :initarg :cookies :initform (make-hash-table :test 'equalp) :accessor cookies)
   (domain :initarg :domain :initform "localhost" :accessor domain)
   (remote :initarg :remote :initform "unknown" :accessor remote)
   (data :initarg :data :initform (make-hash-table :test 'eql) :accessor data)))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type T)
    (format stream "~a ~s" (http-method request) (uri request)))
  request)

(defclass response ()
  ((data :initarg :data :initform NIL :accessor data)
   (return-code :initarg :return-code :initform 200 :accessor return-code)
   (content-type :initarg :content-type :initform *default-content-type* :accessor content-type)
   (external-format :initarg :external-format :initform *default-external-format* :accessor external-format)
   (headers :initarg :headers :initform (make-hash-table :test 'equalp) :accessor headers)
   (cookies :initarg :cookies :initform (make-hash-table :test 'equalp) :accessor cookies)))

(defmethod print-object ((response response) stream)
  (format stream "~s" (data response))
  response)

(defclass cookie ()
  ((name :initarg :name :initform (error "NAME required.") :accessor name)
   (value :initarg :value :initform "" :accessor value)
   (domain :initarg :domain :initform NIL :accessor domain)
   (path :initarg :path :initform NIL :accessor path)
   (expires :initarg :expires :initform NIL :accessor expires)
   (http-only :initarg :http-only :initform NIL :accessor http-only)
   (secure :initarg :secure :initform NIL :accessor secure)))

(defun %cookie-time (stream time atp cp)
  (declare (ignore atp cp))
  (local-time:format-timestring
   stream (local-time:universal-to-timestamp time)
   :format local-time:+rfc-1123-format+))

(defun cookie-header (cookie)
  (format NIL "Set-Cookie: ~a=~a;~@[Domain=~a;~]~@[Path=~a;~]~@[Secure;~*~]~@[HttpOnly;~*~]~@[Expires=~/radiance::%cookie-time/;~]"
          (name cookie) (value cookie) (domain cookie) (path cookie) (secure cookie) (http-only cookie) (expires cookie)))

(defmethod print-object ((c cookie) stream)
  (print-unreadable-object (c stream :type T)
    (format stream "~a=~s ~@[~a~]~@[~a~] (~:[SESSION~;~:*~a~])~:[~; HTTP-ONLY~]~:[~; SECURE~]"
            (name c) (value c) (domain c) (path c) (expires c) (http-only c) (secure c))))

(defun user-agent (&optional (request *request*))
  (gethash "user-agent" (headers request)))

(defun (setf user-agent) (value &optional (request *request*))
  (setf (gethash "user-agent" (headers request)) value))

(defun referer (&optional (request *request*))
  (gethash "referer" (headers request)))

(defun (setf referer) (value &optional (request *request*))
  (setf (gethash "referer" (headers request)) value))

(defun cookie (name &optional (request/response *request*))
  (gethash (string name) (cookies request/response)))

(defun get-var (name &optional (request *request*))
  (gethash (string name) (get-data request)))

(defun post-var (name &optional (request *request*))
  (gethash (string name) (post-data request)))

(defun post/get (name &optional (request *request*))
  (or (gethash (string name) (post-data request))
      (gethash (string name) (get-data request))))

(defun header (name &optional (request/response *request*))
  (gethash (string name) (headers request/response)))

(defmethod field ((request request) field)
  (gethash field (data request)))

(defmethod (setf field) (value (request request) field)
  (setf (gethash field (data request)) value))

(defun file (name &optional (request *request*))
  "Returns file info about a form uploaded file.
 (PATH ORIGINAL-FILENAME MIME-TYPE)"
  (let ((var (post-var name request)))
    (cond
      ((null var) (error "No such post parameter."))
      ((listp var) var)
      (T (error "Post parameter is not a file.")))))

(defun (setf cookie) (value name &key domain path timeout http-only secure (response *response*))
  (setf (gethash name (cookies response))
        (make-instance 'cookie :name name :value value :domain domain :path path :expires timeout :http-only http-only :secure secure)))

(defun (setf header) (value name &optional (response *response*))
  (setf (gethash name (headers response)) value))

(defun redirect (new-address &optional (code 307) (response *response*))
  (setf (return-code response) code)
  (setf (header "Location" response) new-address))

(defun serve-file (pathname &optional content-type (response *response*))
  (setf (content-type response) (or content-type (mimes:mime-lookup pathname) "application/octet-stream"))
  (setf (data response) pathname))

(define-hook request (request response))
(defun execute-request (request &optional (response (make-instance 'response)))
  (handler-bind ((error #'handle-condition))
    (let ((*request* request)
          (*response* response)
          (*session* NIL))
      (restart-case
          (progn
            (trigger 'request request response)
            (let ((result (dispatch (uri request))))
              (typecase result
                (response (setf *response* result))
                (pathname (serve-file result))
                (string (setf (data *response*) result))
                ((array (unsigned-byte 8)) (setf (data *response*) result)))))
        (set-data (data)
          :report "Set the response data."
          :interactive read-value
          (if (typep data 'response)
              (setf *response* data)
              (setf (data *response*) data))))
      *response*)))

(defun ensure-request-hash-table (thing)
  (etypecase thing
    (null
     (make-hash-table :test 'equalp))
    (hash-table
     (case (hash-table-test thing)
       (equalp thing)
       (T (copy-hash-table thing :test 'equalp))))
    (list
     (let ((table (make-hash-table :test 'equalp)))
       (flet ((push-to-table (k v)
                (if (string= "[]" k :start2 (- (length k) 2))
                    (push v (gethash k table))
                    (setf (gethash k table) v))))
         (etypecase (first thing)
           ((or string keyword)
            (loop for (k v) on thing by #'cddr
                  do (push-to-table k v)))
           (cons
            (loop for (k . v) in thing
                  do (push-to-table k v)))))
       table))))

(defun request (to-uri &key (representation :internal) (http-method :GET) headers post get cookies (remote "unknown") (response (make-instance 'response)))
  ;; KLUDGE!
  ;; This should be handled nicer somehow, but
  ;; we currently have to do it like this as we
  ;; would run into a problem because the domain
  ;; cutter route needs to set the DOMAIN on
  ;; *request* and has no other means to
  ;; communicate this information to us. Thus,
  ;; we first spoof the URI and *REQUEST* to
  ;; perform the proper routing and then switch
  ;; out the URIs to dispatch.
  (let ((*request* (make-instance
                    'request
                    :uri to-uri
                    :http-method http-method
                    :headers (ensure-request-hash-table headers)
                    :post-data (ensure-request-hash-table post)
                    :get-data (ensure-request-hash-table get)
                    :cookies (ensure-request-hash-table cookies)
                    :remote remote)))
    (setf (uri *request*) (represent-uri (uri *request*) representation))
    (execute-request
     *request*
     response)))
