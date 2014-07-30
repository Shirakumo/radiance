#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defvar *request*)
(defvar *response*)
(defvar *default-external-format* :UTF-8)

(defclass request (uri)
  ((headers :initarg :headers :initform () :accessor headers)
   (http-method :initarg :http-method :initform "GET" :accessor http-method)
   (post-data :initarg :post-data :initform () :accessor post-data)
   (get-data :initarg :get-data :initform () :accessor get-data)
   (cookies :initarg :cookies :initform () :accessor cookies)
   (remote :initarg :remote :initform "unknown" :accessor remote)))

(defclass response ()
  ((data :initarg :data :initform NIL :accessor data)
   (headers :initarg :headers :initform () :accessor headers)
   (content-type :initarg :content-type :initform "text/plain" :accessor content-type)
   (return-code :initarg :return-code :initform 200 :accessor return-code)
   (external-format :initarg :external-format :initform *default-external-format* :accessor external-format)))

(defmethod print-object ((response response) stream)
  (format stream "~s" (data response)))

(defun request (request &optional (response (make-instance 'response)))
  (let ((*request* request)
        (*response* response))
    (let ((result (dispatch request)))
      (typecase result
        (response (setf *response* result))
        (string (setf (data *response*) result))
        (array (setf (data *response*) result)))
      *response*)))
