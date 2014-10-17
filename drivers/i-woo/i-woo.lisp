#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:i-woo
    (:use #:cl #:radiance)
    (:implements #:server))
(in-package #:i-woo)

(defvar *listeners* (make-hash-table :test 'equalp))
(defvar *url-rewriters* (make-hash-table :test 'equalp))

(defun compile-url-rewriter (url)
  (let* ((domains (nreverse (cl-ppcre:split "\\." url)))
         (len (length domains)))
    #'(lambda (uri)
        (and (loop for domain in domains
                   for compare in (domains uri)
                   always (string-equal domain compare))
             (loop repeat len do (pop (domains uri))
                   finally (return T))))))

(define-trigger server-start ()
  (loop for domain in (config-tree :server :domains)
        do (setf (gethash domain *url-rewriters*)
                 (compile-url-rewriter domain)))
  (loop for config in (config-tree :server :instances)
        do (server:start (gethash :port config)
                         :address (gethash :address config))))

(defun server:start (port &key address ssl-cert ssl-key ssl-pass)
  (when (or ssl-cert ssl-key ssl-pass)
    (warn "SSL is currently not supported in WOO; ignored.")) ;; Until that is supported.
  (let ((name (format NIL "~a:~a" address port)))
    (when (gethash name *listeners*)
      (error "Server already started on specified port & address!"))
    (l:info :server "Starting listener ~a" name)
    (setf (gethash name *listeners*)
          (woo:run #'handle-request :port port :address (or address "0.0.0.0")))
    (trigger 'server:started port address)))

(defun server:stop (port &optional address)
  (let* ((name (format NIL "~a:~a" address port))
         (listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (woo:stop listener)
       (remhash name *listeners*)
       (trigger 'server:stopped port address))
      (T
       (error "No such listener found!")))))

(defun server:listeners ()
  (loop for name being the hash-keys of *listeners*
        collect name))

(defun url-decode (string)
  (when (string= string "")
    (return-from url-decode string))
  (let ((vector (make-array (length string) :element-type '(unsigned-byte 8) :fill-pointer 0)))
    (loop for i from 0 below (length string)
          for char = (aref string i)
          do (case char
               (#\%
                (vector-push (parse-integer string :start (+ i 1) :end (+ i 3) :radix 16) vector)
                (incf i 2))
               (#\+
                (vector-push (char-code #\Space) vector))
               (t
                (vector-push (char-code char) vector))))
    (flexi-streams:octets-to-string vector :external-format :utf-8)))

(defun parse-get (get)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for pair in (cl-ppcre:split "&" get)
          do (let ((pos (position #\= pair)))
               (setf (gethash (url-decode (subseq pair 0 pos)) table)
                     (when pos (url-decode (subseq pair (1+ pos)))))))
    table))

(defun parse-post (body)
  ;; TODO, somehow.
  (make-hash-table :test 'equalp))

(defun parse-headers (env)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (key val) on env by #'cddr
          for name = (string key)
          when (and (< 5 (length name))
                    (string= "HTTP-" name :end2 5))
          do (setf (gethash name table) val))
    table))

(defun parse-cookies (env)
  (let ((table (make-hash-table :test 'equalp)))
    (loop for (key val) on env by #'cddr
          when (eq key :http-cookie)
          do (let ((pos (position #\= val)))
               (setf (gethash (subseq val 0 pos) table)
                     (subseq val (1+ pos)))))
    table))

(defun transform-response (request response)
  (list (return-code response)
        (let ((headers (list :content-type (content-type response))))
          (loop for header being the hash-keys of (headers response)
                for value being the hash-values of (headers response)
                do (push value headers)
                   (push (intern (string-upcase header) "KEYWORD") headers))
          (loop for cookie being the hash-keys of (cookies response)
                for value being the hash-values of (cookies response)
                do (push (cookie-header value) headers)
                   (push (intern (string-upcase cookie) "KEYWORD") headers))
          headers)
        (etypecase (data response)
          (string (list (data response)))
          ((array (unsigned-byte 8)) (data response))
          (pathname (data response))
          (null (error 'request-empty :request request)))))

(defun handle-request (env)
  (destructuring-bind (&key raw-body request-method server-name server-port request-uri query-string &allow-other-keys) env
    (let ((request (make-instance 'request :remote NIL :domain server-name :http-method request-method
                                           :get-data (parse-get query-string) :post-data (parse-post raw-body)
                                           :headers (parse-headers env) :cookies (parse-cookies env)
                                           :path (subseq request-uri 1 (position #\? request-uri)) :port server-port
                                           :domains (nreverse (cl-ppcre:split "\\." server-name)))))
      ;; Cut domain
      (loop for domain being the hash-keys of *url-rewriters*
            for rewriter being the hash-values of *url-rewriters*
            when (funcall rewriter request)
            do (return (setf (domain request) domain)))
      ;; Go
      (let ((response (request request)))
        (transform-response request response)))))
