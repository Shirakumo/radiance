#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *uri-registry* (make-hash-table :test 'eql))
(defvar *uri-priority* (make-array 0))
(defparameter *uri-fallback* #'(lambda ()
                                 (cond
                                   ((boundp '*response*)
                                    (serve-file (data-file "html/error/404.html") "application/xhtml+xml"))
                                   ((boundp '*request*)
                                    (error 'request-not-found :message "Reached dispatch fallback."))
                                   (T
                                    (error 'request-not-found :request NIL :message "Reached dispatch fallback.")))))

(defclass uri-dispatcher (uri)
  ((name :initarg :name :initform (error "NAME required.") :accessor name)
   (dispatch-function :initarg :dispatch-function :initform (error "DISPATCH-FUNCTION required.") :accessor dispatch-function)
   (priority :initarg :priority :initform NIL :accessor priority)))

(defun make-uri-dispatcher (name uri dispatch-function &optional priority)
  (let ((uri (copy-uri uri)))
    (initialize-instance
     (change-class uri 'uri-dispatcher
                   :name name
                   :dispatch-function dispatch-function
                   :priority priority
                   :matcher T))
    uri))

(defun uri-dispatcher (name)
  (gethash name *uri-registry*))

(defun (setf uri-dispatcher) (uri-or-f name &optional uri priority)
  (if uri
      (setf uri-or-f (make-uri-dispatcher name uri uri-or-f priority))
      (unless (typep uri-or-f 'uri-dispatcher)
        (error 'type-error :datum uri-or-f :expected-type 'uri-dispatcher)))
  (setf (gethash name *uri-registry*) uri-or-f)
  (rebuild-uri-priority)
  uri-or-f)

(defun remove-uri-dispatcher (name)
  (remhash name *uri-registry*)
  (rebuild-uri-priority)
  name)

(defun list-uri-dispatchers ()
  (coerce *uri-priority* 'list))

(defun rebuild-uri-priority ()
  (setf *uri-priority*
        (sort (loop with array = (make-array (hash-table-count *uri-registry*))
                    for uri being the hash-values of *uri-registry*
                    for i from 0 do (setf (aref array i) uri)
                    finally (return array))
              #'(lambda (a b)
                  (or (and (priority a)
                           (or (not (priority b))
                               (>= (priority a) (priority b))))
                      (and (not (priority b))
                           (uri> a b)))))))

(defmacro define-uri-dispatcher (name (uri &optional priority) &body body)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (uri-dispatcher ',name ,uri ,priority)
           #'(lambda ()
               ,@body))))

(defun dispatch (uri)
  (loop for dispatcher across *uri-priority*
        when (uri-matches uri dispatcher)
          do (return (funcall (dispatch-function dispatcher)))
        finally (return (funcall *uri-fallback*))))
