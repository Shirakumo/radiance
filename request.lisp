#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defvar *request*)
(defvar *response*)
(defvar *default-external-format* :UTF-8)
(defvar *default-content-type* "text/plain")

(defclass request (uri)
  ((http-method :initarg :http-method :initform "GET" :accessor http-method)
   (headers :initarg :headers :initform (make-hash-table :test 'equalp) :accessor headers)
   (post-data :initarg :post-data :initform (make-hash-table :test 'equalp) :accessor post-data)
   (get-data :initarg :get-data :initform (make-hash-table :test 'equalp) :accessor get-data)
   (cookies :initarg :cookies :initform (make-hash-table :test 'equalp) :accessor cookies)
   (user-agent :initarg :user-agent :initform "unknown" :accessor user-agent)
   (referer :initarg :referer :initform "" :accessor referer)
   (remote :initarg :remote :initform "unknown" :accessor remote)))

(defclass response ()
  ((data :initarg :data :initform NIL :accessor data)
   (return-code :initarg :return-code :initform 200 :accessor return-code)
   (content-type :initarg :content-type :initform *default-content-type* :accessor content-type)
   (external-format :initarg :external-format :initform *default-external-format* :accessor external-format)
   (headers :initarg :headers :initform (make-hash-table :test 'equalp) :accessor headers)
   (cookies :initarg :cookies :initform (make-hash-table :test 'equalp) :accessor cookies)))

(defmethod print-object ((response response) stream)
  (format stream "~s" (data response)))

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
    (format stream "~a=~s ~@[~a~]~@[/~a~] (~:[FOREVER~;~:*~a~])~:[~; HTTP-ONLY~]~:[~; SECURE~]"
            (name c) (value c) (domain c) (path c) (expires c) (http-only c) (secure c))))

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

(defun (setf cookie) (value name &key domain path expires http-only secure (response *response*))
  (setf (gethash name (cookies response))
        (make-instance 'cookie :name name :value value :domain domain :path path :expires expires :http-only http-only :secure secure)))

(defun (setf header) (value name &optional (response *response*))
  (setf (gethash name (headers response)) value))

(defun redirect (new-address &optional (code 301) (response *response*))
  (setf (header "Location" response) new-address)
  (setf (return-code response) code))

(defun request (request &optional (response (make-instance 'response)))
  (let ((*request* request)
        (*response* response))
    (let ((result (dispatch request)))
      (typecase result
        (response (setf *response* result))
        (string (setf (data *response*) result))
        (array (setf (data *response*) result)))
      *response*)))
