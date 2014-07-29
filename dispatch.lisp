#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

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
   (external-format :initarg :external-format :initform :UTF-8 :accessor external-format)))

(defmethod print-object ((response response) stream)
  (format stream "~s" (data response)))

(defvar *uri-registry* (make-hash-table :test 'eql))
(defvar *uri-priority* (make-array 0))

(defclass uri-dispatcher (uri)
  ((dispatch-function :initarg :dispatch-function :initform (constantly t) :accessor dispatch-function)))

(defun uri-dispatcher (name)
  (gethash name *uri-registry*))

(defun (setf uri-dispatcher) (uri-or-f name &optional uri)
  (if uri
      (setf uri-or-f (make-uri-dispatcher uri uri-or-f))
      (unless (typep uri-or-f 'uri-registry)
        (error 'type-error :datum uri-or-f :expected-type 'uri-registry)))
  (setf (gethash name *uri-registry*) uri-or-f)
  (rebuild-uri-priority))

(defun make-uri-dispatcher (uri dispatch-function)
  (let ((uri (copy-uri uri)))
    (change-class uri 'uri-dispatcher)
    (setf (dispatch-function uri) dispatch-function)
    uri))

(defun rebuild-uri-priority ()
  (setf *uri-priority*
        (sort (loop with array = (make-array (hash-table-count *uri-regex*))
                    for uri being the hash-values of *uri-registry*
                    for i from 0 do (setf (aref array i) uri)) #'uri>)))

(defmacro define-uri-dispatcher (name (uri &optional (requestvar 'request)) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (uri-dispatcher ',name ,uri) #'(lambda (,requestvar) ,@body))))

(defun dispatch (request)
  (loop for uri across *uri-priority*
        when (uri-matches request uri)
          do (return (funcall (dispatch-function uri) request))))
