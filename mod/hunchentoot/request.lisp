#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass request (server:request hunchentoot:request) ())

(defmethod print-object ((request request) out)
  (print-unreadable-object (request out :type T)
    (format out "~a:~a → (~a /~a)" (domain request) (port request) (subdomains request) (path request))))

(defclass response (server:response hunchentoot:reply)
  ((%body :initform NIL :accessor body)))

(defun parse-request (request)
  (declare (optimize (speed 3) (safety 0)))
  (let ((path (hunchentoot:script-name request))
        (host (hunchentoot:host request))
        (domain (config-tree :domain))
        (port 0) subdomains)
    (declare (simple-string path host domain))
    (declare (fixnum port))
    (declare (list subdomains))
    
    (setf path (string-left-trim "/" path))
    (let ((searchpos (search ":" host)))
      (declare (type (or fixnum null) searchpos))
      (if searchpos
          (setf port (parse-integer (subseq host (1+ searchpos)))
                host (subseq host 0 searchpos))
          (setf port 80)))

    (if (string= domain "autodetect")
        (let ((searchpos (search "." host :from-end T)))
          (declare (type (or fixnum null) searchpos))
          (if searchpos
              (let ((searchpos (search "." host :from-end T :end2 searchpos)))
                (declare (type (or fixnum null) searchpos))
                (if searchpos
                    (setf subdomains (split-sequence:split-sequence #\. (subseq host 0 searchpos))
                          domain (subseq host (1+ searchpos)))
                    (setf subdomains NIL
                          domain host)))
              (setf subdomains NIL
                    domain host)))
        (let ((searchpos (- (length host) (length domain))))
          (if (> searchpos 0)
              (setf subdomains (split-sequence:split-sequence #\. (subseq host 0 (1- searchpos))))
              (setf subdomains NIL))))

    (when (not (config-tree :use-subdomains))
                                        ;TODO
      )

    (setf (subdomains request) subdomains
          (domain request) domain
          (port request) port
          (path request) path)))