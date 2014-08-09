#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(defclass uri ()
  ((domains :initarg :domains :initform () :accessor domains)
   (port :initarg :port :initform NIL :accessor port)
   (path :initarg :path :initform NIL :accessor path)
   (matcher :initarg :matcher :accessor matcher)))

(defmethod initialize-instance :after ((uri uri) &key)
  (unless (slot-boundp uri 'matcher)
    (setf (matcher uri) (cl-ppcre:create-scanner (or (path uri) "")))))

(defmethod print-object ((uri uri) stream)
  (format stream "#U\"~{~a~^.~}~@[:~a~]/~@[~a~]\""
          (reverse (domains uri)) (port uri) (path uri)))

(defmethod (setf path) (val (uri uri))
  (when (matcher uri)
    (setf (matcher uri) (cl-ppcre:create-scanner (or (path uri) "")))))

(defun make-uri (&key domains port path (matcher NIL m-p))
  (if m-p
      (make-instance 'uri :domains domains :port port :path path :matcher matcher)
      (make-instance 'uri :domains domains :port port :path path)))

(defun copy-uri (uri)
  (make-uri :domains (copy-seq (domains uri))
            :port (port uri)
            :path (path uri)))

(defvar *uri-regex* (cl-ppcre:create-scanner "^(([a-z0-9\\-]+\\.)*[a-z0-9\\-]+)?(:(\\d{1,5}))?/(.*)" :case-insensitive-mode T))
(defun parse-uri (uri-string)
  (or (cl-ppcre:register-groups-bind (domains NIL NIL port path) (*uri-regex* uri-string)
        (make-uri :domains (nreverse (cl-ppcre:split "\\." domains)) :port (when port (parse-integer port)) :path path))
      (error "Failed to parse URI.")))

(set-dispatch-macro-character
 #\# #\U
 #'(lambda (stream char arg)
     (declare (ignore char arg))
     `(parse-uri ,(read stream))))

(defun uri< (a b)
  (or (and (not (port a)) (port b))
      (and (not (port a))
           (or (< (length (domains a)) (length (domains b)))
               (and (= (length (domains a)) (length (domains b)))
                    (< (length (path a)) (length (path b))))))))

(defun uri> (a b)
  (uri< b a))

(defun uri= (a b)
  (and (eql (port a) (port b))
       (equal (path a) (path b))
       (= (length (domains a)) (length (domains b)))
       (loop for a in (domains a)
             for b in (domains b)
             always (string-equal a b))))

(defvar *default-uri-defaults* (parse-uri "/"))
(defun uri-matches (uri pattern-uri)
  (and (or (not (port pattern-uri))
           (not (port uri))
           (= (port uri) (port pattern-uri)))
       (<= (length (domains pattern-uri)) (length (domains uri)))
       (loop for a in (domains uri)
             for b in (domains pattern-uri)
             always (string-equal a b))
       (cl-ppcre:scan (matcher pattern-uri) (or (path uri) ""))))

(defun merge-uris (uri &optional (defaults *default-uri-defaults*))
  (make-uri
   :domains (append (domains uri) (domains defaults))
   :port (or (port uri) (port defaults))
   :path (format NIL "~@[~a/~]~@[~a~]"
                 (path defaults) (path uri))))

(defun uri-to-string (uri &key print-port print-request-domain)
  (format NIL "~{~a~^.~}~:[~*~;~a~]~:[~*~;:~a~]/~a"
          (domains uri) (and print-request-domain (typep uri 'request)) (domain uri)
          (and print-port (port uri)) (port uri) (path uri)))
