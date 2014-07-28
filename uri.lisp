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
  (setf (matcher uri) (cl-ppcre:create-scanner (or (path uri) "") :case-insensitive-mode T)))

(defmethod print-object ((uri uri) stream)
  (format stream "#U\"~{~a~^.~}~@[:~a~]/~@[~a~]\""
          (domains uri) (port uri) (path uri)))

(defun make-uri (&key domains port path)
  (make-instance
   'uri
   :domains domains
   :port port
   :path path))

(defvar *uri-regex* (cl-ppcre:create-scanner "^(([a-z0-9\\-]+\\.)*[a-z0-9\\-]+)?(:(\\d{1,5}))?/(.*)" :case-insensitive-mode T))
(defun parse-uri (uri-string)
  (or (cl-ppcre:register-groups-bind (domains NIL NIL port path) (*uri-regex* uri-string)
        (make-uri :domains (cl-ppcre:split "\\." domains) :port (when port (parse-integer port)) :path path))
      (error "Failed to parse URI.")))

(set-dispatch-macro-character
 #\# #\U
 #'(lambda (stream char arg)
     (declare (ignore char arg))
     (parse-uri (read stream))))

(defun uri< (a b)
  (or (and (not (port a)) (port b))
      (and (not (port a))
           (or (< (length (domains a)) (length (domains b)))
               (and (= (length (domains a)) (length (domains b)))
                    (< (length (path a)) (length (path b))))))))

(defun uri> (b a)
  (or (and (not (port a)) (port b))
      (and (not (port a))
           (or (< (length (domains a)) (length (domains b)))
               (and (= (length (domains a)) (length (domains b)))
                    (< (length (path a)) (length (path b))))))))

(defun uri= (a b)
  (and (eql (port a) (port b))
       (equal (path a) (path b))
       (= (length (domains a)) (length (domains b)))
       (loop for a in (domains a)
             for b in (domains b)
             always (string-equal a b))))

(defun uri-matches (uri pattern-uri)
  (and (or (not (port pattern-uri))
           (not (port uri))
           (= (port uri) (port pattern-uri)))
       (<= (length (domains pattern-uri)) (length (domains uri)))
       (loop for a in (reverse (domains uri))
             for b in (reverse (domains pattern-uri))
             always (string-equal a b))
       (cl-ppcre:scan (matcher pattern-uri) (or (path uri) ""))))
