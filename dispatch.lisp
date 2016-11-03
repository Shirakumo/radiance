#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(declaim (ftype (function (uri-dispatcher) function) dispatch-function))
(eval-when (:compile-toplevel :load-toplevel :execute)
  (define-documentable uri-dispatcher (uri)
    ((name :initarg :name :accessor name)
     (dispatch-function :initarg :dispatch-function :accessor dispatch-function)
     (priority :initarg :priority :accessor priority))
    (:default-initargs
     :name (error "NAME required.")
     :dispatch-function (error "DISPATCH-FUNCTION required.")
     :priority NIL)
    (:find-function uri-dispatcher)))

(defvar *uri-registry* (make-hash-table :test 'eql))
(defvar *uri-priority* (make-array 0 :element-type 'uri-dispatcher))
(defparameter *uri-fallback* (lambda ()
                               (cond
                                 ((boundp '*request*)
                                  (error 'request-not-found :message "Reached dispatch fallback."))
                                 (T
                                  (error 'request-not-found :request NIL :message "Reached dispatch fallback.")))))
(declaim (function *uri-fallback*))
(declaim ((simple-array uri-dispatcher 1) *uri-priority*))

(defmethod print-object ((dispatcher uri-dispatcher) stream)
  (print-unreadable-object (dispatcher stream :type T)
    (format stream "~a " (name dispatcher))
    (call-next-method)))

(defun uri-dispatcher (name)
  (gethash name *uri-registry*))

(defun (setf uri-dispatcher) (dispatcher name)
  (setf (gethash name *uri-registry*) dispatcher)
  (rebuild-uri-priority)
  dispatcher)

(defun remove-uri-dispatcher (name)
  (remhash name *uri-registry*)
  (rebuild-uri-priority)
  name)

(defun list-uri-dispatchers ()
  (coerce *uri-priority* 'list))

(defun uri-dispatcher> (a b)
  (or (and (priority a)
           (or (not (priority b))
               (>= (priority a) (priority b))))
      (and (not (priority b))
           (uri> a b))))

(defun rebuild-uri-priority ()
  (setf *uri-priority*
        (sort (make-array (hash-table-count *uri-registry*)
                          :element-type 'uri-dispatcher
                          :initial-contents (loop for uri being the hash-values of *uri-registry*
                                                  collect uri))
              #'uri-dispatcher>)))

(defmacro define-uri-dispatcher (name (uri &optional priority) &body body)
  (let ((dispatcher (gensym "DISPATCHER")))
    `(eval-when (:compile-toplevel :load-toplevel :execute)
       (flet ((,dispatcher ()
                ,@body))
         (setf (uri-dispatcher ',name)
               (change-class (copy-uri ,uri)
                             'uri-dispatcher
                             :name ',name
                             :dispatch-function #',dispatcher
                             :priority ,priority
                             :matcher T))))))

(defun dispatch (uri)
  (declare (optimize speed))
  (loop for dispatcher across *uri-priority*
        when (uri-matches uri dispatcher)
        do (return (funcall (dispatch-function dispatcher)))
        finally (return (funcall *uri-fallback*))))
