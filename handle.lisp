#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *debugger*)

(defun swank-connected-p ()
  (let ((threads (bt:all-threads)))
    (and (find-package :swank)
         (find "repl-thread" threads :key #'bt:thread-name :test #'equal)
         (find "reader-thread" threads :key #'bt:thread-name :test #'equal)
         (find "control-thread" threads :key #'bt:thread-name :test #'equal))))

(defun maybe-invoke-debugger (condition &optional restart &rest values)
  (when (case *debugger*
          (:if-swank-connected (swank-connected-p))
          ((T) T))
    (with-simple-restart (continue "Don't handle ~a." condition)
      (invoke-debugger condition)))
  (when restart
    (apply #'invoke-restart restart values)))

(defun handle-condition (condition)
  (if (typep condition 'radiance-condition)
      (l:debug :radiance condition)
      (l:warn :radiance condition))
  (l:warn :radiance "Handling stray condition: ~a" condition)
  (restart-case
      (maybe-invoke-debugger condition 'abort)
    (abort ()
      :report "Render error page."
      (invoke-restart 'set-data (render-error-page condition)))))

(defun render-error-page (condition)
  (setf (return-code *response*) 500)
  (setf (content-type *response*) "text/plain")
  (format NIL "Internal error: ~s" condition))

;; FIXME: Add support for STREAMs in data of response to the implementations.
;; FIXME: Access to the input stream of the body without parsing, somehow?
(define-hook request (request response))
(defun execute-request (request &optional (response (make-instance 'response)))
  (declare (optimize (speed 3)))
  (l:trace :core.request "Executing request: ~s ~s" request response)
  (handler-bind ((error #'handle-condition))
    (let ((*request* request)
          (*response* response))
      (restart-case
          (progn
            (trigger 'request request response)
            (let ((result (dispatch (uri request))))
              (typecase result
                (string (setf (data *response*) result))
                (pathname (serve-file result))
                ((array (unsigned-byte 8)) (setf (data *response*) result))
                (stream (setf (data *response*) result))
                (response (setf *response* result)))))
        (set-data (data)
          :report "Set the response data."
          :interactive read-value
          (if (typep data 'response)
              (setf *response* data)
              (setf (data *response*) data))))
      (values *response* *request*))))

(defun ensure-request-hash-table (thing)
  (declare (optimize (speed 3)))
  (etypecase thing
    (null
     (make-hash-table :test 'equalp))
    (hash-table
     (case (hash-table-test thing)
       (equalp thing)
       (T (copy-hash-table thing :test 'equalp))))
    (list
     (let ((table (make-hash-table :test 'equalp))
           (to-reverse ()))
       (flet ((push-to-table (k v)
                (let ((k (string k)))
                  (cond ((ends-with "[]" k)
                         (push v (gethash k table))
                         (pushnew k to-reverse :test #'equalp))
                        (T
                         (setf (gethash k table) v))))))
         (etypecase (first thing)
           ((or string keyword)
            (loop for (k v) on thing by #'cddr
                  do (push-to-table k v)))
           (cons
            (loop for (k . v) in thing
                  do (push-to-table k v)))))
       (dolist (k to-reverse)
         (setf (gethash k table) (nreverse (gethash k table))))
       table))))

(defun request (to-uri &key (representation :internal) (http-method :GET) body-stream headers post get cookies (remote "unknown") (response (make-instance 'response)))
  (declare (optimize (speed 3)))
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
                    :body-stream body-stream
                    :headers (ensure-request-hash-table headers)
                    :post-data (ensure-request-hash-table post)
                    :get-data (ensure-request-hash-table get)
                    :cookies (ensure-request-hash-table cookies)
                    :remote remote)))
    (l:trace :core.request "Received request ~a" *request*)
    (setf (uri *request*) (represent-uri (uri *request*) representation))
    (execute-request
     *request*
     response)))
