#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-wookie)

(defvar *listeners* (make-hash-table :test 'equalp))
(defvar *listener-lock* (bt:make-lock "LISTENERS"))

(define-trigger (server-start 'launch-listeners) ()
  (loop for config in (config-tree :server :instances)
        do (server:start (gethash :port config)
                         :address (gethash :address config)
                         :ssl-key (whenthen (gethash :ssl-key config) #'data-file)
                         :ssl-cert (whenthen (gethash :ssl-cert config) #'data-file)
                         :ssl-pass (gethash :ssl-pass config))))

(define-trigger server-stop ()
  (loop for name being the hash-keys of *listeners*
        do (let* ((pos (position #\: name))
                  (port (parse-integer (subseq name (1+ pos))))
                  (address (subseq name 0 pos)))
             (server:stop port (unless (string= address "NIL") address)))))

(defun server:start (port &key address ssl-cert ssl-key ssl-pass)
  (let ((name (format NIL "~a:~a" address port)))
    (l:info :server "Starting listener ~a" name)
    (bt:make-thread
     #'(lambda ()
         (as:with-event-loop (:catch-app-errors T)
           (let* ((listener (if (and ssl-cert ssl-key)
                                (make-instance 'wookie:ssl-listener :port port :bind address :password ssl-pass :key ssl-key :certificate ssl-cert)
                                (make-instance 'wookie:listener :port port :bind address)))
                  (server (wookie:start-server listener)))
             (bt:with-lock-held (*listener-lock*)
               (setf (gethash name *listeners*) server))))))
    (trigger 'server:started port address)))

(defun server:stop (port &optional address)
  (let* ((name (format NIL "~a:~a" address port))
         (listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (bt:with-lock-held (*listener-lock*)
         (as:close-tcp-server listener)
         (remhash name *listeners*))
       (trigger 'server:stopped port address))
      (T
       (error "No such listener found.")))))

(defun server:listeners ()
  (bt:with-lock-held (*listener-lock*)
    (loop for name being the hash-keys of *listeners*
          collect name)))

(defun handle-request (request wk-response)
  (l:trace :server "Handle: ~a" request)
  (let ((response (request request)))
    (handler-bind ((error #'handle-condition))
      ;; Process attributes
      (maphash #'(lambda (key val) (declare (ignore key)) (set-real-cookie val wk-response)) (cookies response))
      (maphash #'(lambda (key val)
                   (push val (wookie:response-headers wk-response))
                   (push key (wookie:response-headers wk-response))) (headers response))
      ;; Process body
      (etypecase (data response)
        (pathname
         (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8)))
               (output-stream (wookie:start-response wk-response :status (return-code response))))
           (with-open-file (input-stream (data response) :element-type '(unsigned-byte 8))
             (loop for n = (read-sequence buffer input-stream)
                   while (< 0 n) do
                     (write-sequence (subseq buffer 0 n) output-stream)
                     (force-output output-stream)))
           (wookie:finish-response wk-response)))
        
        ((or string (array (unsigned-byte 8)))
         (wookie:send-response wk-response :status (return-code response) :body (data response)))

        (null (error 'request-empty :request request)))
      wk-response)))

(wookie:defroute (:* ".*") (req res)
  (handler-bind ((error #'(lambda (err)
                            (cond
                              (*debugger* (invoke-debugger err))
                              (t (l:severe :server "Error at lowest level: ~a" err)
                                 (invoke-restart 'error-out))))))
    (restart-case 
        (handle-request (create-real-request req) res)
      (error-out ()
        :report "Just error out ungracefully."
        (wookie:send-response res :status 500 :body "Something went very, very wrong.")))))
