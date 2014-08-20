#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defvar *request*)
(defvar *response*)
(defvar *session*)
(defvar *default-external-format* :UTF-8)
(defvar *default-content-type* "text/html")

(defclass request (uri)
  ((http-method :initarg :http-method :initform :GET :accessor http-method)
   (headers :initarg :headers :initform (make-hash-table :test 'equalp) :accessor headers)
   (post-data :initarg :post-data :initform (make-hash-table :test 'equalp) :accessor post-data)
   (get-data :initarg :get-data :initform (make-hash-table :test 'equalp) :accessor get-data)
   (cookies :initarg :cookies :initform (make-hash-table :test 'equalp) :accessor cookies)
   (domain :initarg :domain :initform "localhost" :accessor domain)
   (remote :initarg :remote :initform "unknown" :accessor remote)))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type T)
    (format stream "~a ~A" (http-method request) (uri-to-string request :print-request-domain T :print-port t)))
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
  (gethash name (cookies request/response)))

(defun get-var (name &optional (request *request*))
  (gethash name (get-data request)))

(defun post-var (name &optional (request *request*))
  (gethash name (post-data request)))

(defun post/get (name &optional (request *request*))
  (or (gethash name (post-data request))
      (gethash name (get-data request))))

(defun header (name &optional (request/response *request*))
  (gethash name (headers request/response)))

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

(defun redirect (new-address &optional (code 301) (response *response*))
  (setf (return-code response) code)
  (setf (header "Location" response) new-address))

(defun serve-file (pathname &optional content-type (response *response*))
  (setf (content-type response) (or content-type (mimes:mime-lookup pathname)))
  (setf (data response) pathname))

(define-hook request (request response))
(defun request (request &optional (response (make-instance 'response)))
  (handler-bind ((error #'handle-condition))
    (let ((*request* (resolve-route request))
          (*response* response)
          (*session* NIL))
      (trigger 'request request response)
      (restart-case
          (let ((result (dispatch request)))
            (typecase result
              (response (setf *response* result))
              (string (setf (data *response*) result))
              ((array (unsigned-byte 8)) (setf (data *response*) result))))
        (set-data (data)
          :report "Set a new data"
          :interactive read-value
          (setf (data *response*) data)))
      (loop until
            (restart-case
                (etypecase (data *response*)
                  (pathname T) (string T) ((array (unsigned-byte 8)) T)
                  (null (error 'request-empty :request *request*)))
              (set-data (data)
                :report "Set new data"
                :interactive read-value
                (setf (data *response*) data)
                NIL)))
      *response*)))
