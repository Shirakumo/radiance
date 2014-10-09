#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module i-hunchentoot
    (:use #:cl #:radiance)
    (:implements #:server))
(in-package #:i-hunchentoot)

(defvar *listeners* (make-hash-table :test 'equalp))
(defvar *url-rewriters* (make-hash-table :test 'equalp))

(defun whenthen (var func)
  (when var (funcall func var)))

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

(defun mklist (port address ssl-cert ssl-key ssl-pass)
  (let ((args `(:port ,port :address ,address
                :access-log-destination NIL
                :message-log-destination NIL)))
    (if (and ssl-cert ssl-key)
        (apply #'make-instance 'hunchentoot:ssl-acceptor :persistent-connections-p NIL
               :ssl-certificate-file ssl-cert :ssl-privatekey-file ssl-key :ssl-privatekey-password ssl-pass args)
        (apply #'make-instance 'hunchentoot:easy-acceptor :persistent-connections-p NIL args))))

(defun server:start (port &key address ssl-cert ssl-key ssl-pass)
  (let ((listener (mklist port address ssl-cert ssl-key ssl-pass))
        (name (format NIL "~a:~a" address port)))
    (l:info :server "Starting listener ~a" name)
    (setf (gethash name *listeners*) listener)
    (hunchentoot:start listener)
    (trigger 'server:started port address)))

(defun server:stop (port &optional address)
  (let* ((name (format NIL "~a:~a" address port))
         (listener (gethash name *listeners*)))
    (cond
      (listener
       (l:info :server "Stopping listener ~a" name)
       (hunchentoot:stop listener)
       (remhash name *listeners*)
       (trigger 'server:stopped port address))
      (T
       (error "No such listener found.")))))

(defun server:listeners ()
  (loop for name being the hash-keys of *listeners*
        collect name))

(defun populate-table-from-alist (table alist)
  (loop for (key . val) in alist
        do (let ((key (string key)))
             (if (and (< 1 (length key)) (string= key "[]" :start1 (- (length key) 2)))
                 (push val (gethash key table))
                 (setf (gethash key table) val))))
  table)

(defun create-real-request (ht-request)
  (let ((request (parse-uri (format NIL "~a~a"
                                    (hunchentoot:host ht-request)
                                    (hunchentoot:url-decode
                                     (hunchentoot:script-name ht-request)
                                     *default-external-format*)))))
    (change-class
     request 
     'request
     :http-method (hunchentoot:request-method ht-request)
     :remote (hunchentoot:remote-addr ht-request))
    (loop for domain being the hash-keys of *url-rewriters*
          for rewriter being the hash-values of *url-rewriters*
          when (funcall rewriter request)
            do (return (setf (domain request) domain)))
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
    #+sbcl (setf (sb-thread:thread-name (bt:current-thread))
                 (princ-to-string request))
    (l:trace :server "Pre-process: ~a" request)
    (let ((response (request request)))
      #'(lambda () (post-handler response request)))))

(setf hunchentoot:*dispatch-table* (list #'pre-handler))


