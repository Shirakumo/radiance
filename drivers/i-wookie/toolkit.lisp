#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-wookie)

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

(define-trigger (server-start 'load-url-rewriters) ()
  (loop for domain in (config-tree :server :domains)
        do (setf (gethash domain *url-rewriters*)
                 (compile-url-rewriter domain))))

(defun whenthen (var func)
  (when var (funcall func var)))

(defun populate-table-from-plist (table alist)
  (loop for (key val) on alist by #'cddr
        do (setf (gethash (string-downcase key) table) val))
  table)

(defun merge-hash-tables (target source &optional (transform #'identity))
  (when (and source target)
    (maphash #'(lambda (k v) (setf (gethash k target) (funcall transform v))) source))
  target)

(defun set-real-cookie (cookie wk-response)
  (wookie-plugin-export:set-cookie
   wk-response
   (name cookie)
   (value cookie)
   :expires (expires cookie)
   :path (path cookie)
   :domain (domain cookie)
   :secure (secure cookie)
   :http-only (http-only cookie)))

(defun create-real-request (wk-request)
  (let ((request (make-instance 'request :http-method (wookie:request-method wk-request))))
    ;; Populate headers
    (populate-table-from-plist (headers request) (wookie:request-headers wk-request))
    ;; Parse URI
    (let ((uri (parse-uri (format NIL "~a~a"
                                  (gethash "host" (headers request))
                                  (urlencode:urldecode
                                   (subseq (wookie:request-resource wk-request) 0
                                           (position #\? (wookie:request-resource wk-request)))
                                   :lenientp T)))))
      (v:info :test "~a" (path uri))
      (setf (domains request) (domains uri)
            (port request) (port uri)
            (path request) (path uri)))
    ;; Populate COOKIES
    (merge-hash-tables (cookies request) (wookie:plugin-request-data :cookie wk-request))
    ;; Populate GET
    (merge-hash-tables (get-data request) (wookie:plugin-request-data :get wk-request))
    ;; Populate POST/FORM
    (cond ((search "application/x-www-form-urlencoded" (gethash "content-type" (headers request)))
           (merge-hash-tables (post-data request) (wookie:plugin-request-data :post wk-request)))
          ((search "multipart/form-data" (gethash "content-type" (headers request)))
           (merge-hash-tables (post-data request) (getf (wookie:plugin-request-data :multipart wk-request) :hash-form))
           (merge-hash-tables (post-data request) (getf (wookie:plugin-request-data :multipart wk-request) :hash-file)
                              #'(lambda (k) (destructuring-bind (&key filename tmp-file mime-type) k
                                              (list (uiop:parse-native-namestring tmp-file)
                                                    filename mime-type))))))
    ;; Populate other garbage
    (setf (remote request) "unknown") ;; wtf how
    ;; Translate domain
    (loop for domain being the hash-keys of *url-rewriters*
          for rewriter being the hash-values of *url-rewriters*
          when (funcall rewriter request)
            do (return (setf (domain request) domain)))
    ;; Done
    request))
