#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module i-hunchentoot
    (:use #:cl #:radiance)
    (:implements #:server))
(in-package #:i-hunchentoot)

(defvar *listeners* (make-hash-table :test 'equalp))

(defun server:start (port &optional address)
  (let ((listener (make-instance 'hunchentoot:easy-acceptor
                                 :port port :address address
                                 :access-log-destination NIL
                                 :message-log-destination NIL))
        (name (format NIL "~a:~a" address port)))
    (l:info :server "Starting listener ~a" name)
    (setf (gethash name *listeners*) listener)
    (hunchentoot:start listener)))

(defun server:stop (port &optional address)
  (let* ((name (format NIL "~a:~a" address port))
         (listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (hunchentoot:stop listener))
      (T
       (error "No such listener found.")))))

(defun server:listeners ()
  (loop for name being the hash-keys of *listeners*
        collect name))

(defun populate-table-from-alist (table alist)
  (loop for (key . val) in alist
        do (setf (gethash key table) val))
  table)

(defun create-real-request (ht-request)
  (let ((request (parse-uri (format NIL "~a~a"
                                    (hunchentoot:host ht-request)
                                    (hunchentoot:request-uri ht-request)))))
    (change-class
     request 
     'request
     :http-method (hunchentoot:request-method ht-request)
     :user-agent (hunchentoot:user-agent ht-request)
     :referer (hunchentoot:referer ht-request)
     :remote (hunchentoot:remote-addr ht-request))
    (populate-table-from-alist (headers request) (hunchentoot:headers-in ht-request))
    (populate-table-from-alist (post-data request) (hunchentoot:post-parameters ht-request))
    (populate-table-from-alist (get-data request) (hunchentoot:get-parameters ht-request))
    (populate-table-from-alist (cookies request) (hunchentoot:cookies-in ht-request))
    request))

(defun set-real-cookie (cookie)
  (hunchentoot:set-cookie
   (name cookie) :value (value cookie) :expires (expires cookie)
                 :path (path cookie) :domain (domain cookie)
                 :secure (secure cookie) :http-only (http-only cookie)))

(defun post-handler (response request)
  (handler-bind
      ((error #'handle-condition))
    (l:trace :server "Post-process: ~a" response)
    ;; Process attributes
    (setf (hunchentoot:return-code*) (return-code response)
          (hunchentoot:content-type*) (content-type response))
    (maphash #'(lambda (key val) (declare (ignore key)) (set-real-cookie val)) (cookies response))
    (maphash #'(lambda (key val) (setf (hunchentoot:header-out key) val)) (headers response))
    ;; Process body
    (etypecase (data response)
      (pathname (hunchentoot:handle-static-file (data response) (content-type response)))
      (string (data response))
      ((array (unsigned-byte 8)) (data response))
      (null (error 'request-empty :request request)))))

(defun pre-handler (request)
  (let ((request (create-real-request request)))
    (l:trace :server "Pre-process: ~a" request)
    (let ((response (request request)))
      #'(lambda () (post-handler response request)))))

(setf hunchentoot:*dispatch-table* (list #'pre-handler))
