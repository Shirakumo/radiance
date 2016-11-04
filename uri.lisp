#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(declaim (ftype (function (T) (or (integer 0 65535) null)) port))
(declaim (ftype (function ((or (integer 0 65535) null) T) T) (setf port)))
(defclass uri ()
  ((domains :initarg :domains :accessor domains)
   (port :initarg :port :accessor port)
   (path :initarg :path :accessor path)
   (matcher :initarg :matcher :accessor matcher))
  (:default-initargs
   :domains ()
   :port NIL
   :path NIL
   :matcher NIL))

(defmethod shared-initialize :after ((uri uri) slots &key)
  (declare (ignore slots))
  (when (eql (matcher uri) T)
    (setf (matcher uri) (cl-ppcre:create-scanner (or (path uri) "")))))

(defun uri-string (uri)
  (format NIL "~{~a~^.~}~@[:~a~]/~@[~a~]"
          (reverse (domains uri)) (port uri) (path uri)))

(defmethod print-object ((uri uri) stream)
  (format stream "#@~s" (uri-string uri))
  uri)

(defmethod make-load-form ((uri uri) &optional env)
  (declare (ignore env))
  `(make-uri :domains ',(domains uri) :port ,(port uri) :path ,(path uri)))

(defmethod (setf path) :after (val (uri uri))
  (when (matcher uri)
    (setf (matcher uri) (cl-ppcre:create-scanner (or (path uri) "")))))

(declaim (inline make-uri))
(defun make-uri (&key domains port path matcher)
  (make-instance 'uri :domains domains :port port :path path :matcher matcher))

(declaim (inline ensure-uri))
(defun ensure-uri (uri-ish)
  (etypecase uri-ish
    (uri uri-ish)
    (string (parse-uri uri-ish))))

(defun copy-uri (uri)
  (etypecase uri
    (uri (make-uri :domains (copy-seq (domains uri))
                   :port (port uri)
                   :path (path uri)
                   :matcher (matcher uri)))
    (string (parse-uri uri))))

(defvar *uri-regex* (cl-ppcre:create-scanner "^(([a-z0-9\\-]+\\.)*[a-z0-9\\-]+)?(:(\\d{1,5}))?/(.*)" :case-insensitive-mode T))
(defun parse-uri (uri-string)
  (or (cl-ppcre:register-groups-bind (domains NIL NIL port path) (*uri-regex* uri-string)
        (make-uri :domains (when domains (nreverse (cl-ppcre:split "\\." (string-downcase domains))))
                  :port (when port (parse-integer port))
                  :path path))
      (error 'unparsable-uri-string :string uri-string)))

(defun read-uri (stream char arg)
  (declare (ignore char arg))
  (parse-uri (read stream)))

(set-dispatch-macro-character #\# #\@ #'read-uri)
(defvar *default-uri-defaults* (parse-uri "/"))

(defun uri< (a b)
  (let ((a (ensure-uri a))
        (b (ensure-uri b)))
    (or (and (not (port a)) (port b))
        (and (not (port a))
             (or (< (length (domains a)) (length (domains b)))
                 (and (= (length (domains a)) (length (domains b)))
                      (< (length (path a)) (length (path b)))))))))

(defun uri> (a b)
  (uri< b a))

(defun uri= (a b)
  (let ((a (ensure-uri a))
        (b (ensure-uri b)))
    (and (eql (port a) (port b))
         (equal (path a) (path b))
         (= (length (domains a)) (length (domains b)))
         (loop for a in (domains a)
               for b in (domains b)
               always (string-equal a b)))))

(defun uri-matches (uri pattern-uri)
  (declare (optimize (speed 3)))
  (let ((uri (ensure-uri uri))
        (pattern-uri (ensure-uri pattern-uri)))
    (unless (matcher pattern-uri)
      (setf (matcher pattern-uri) (cl-ppcre:create-scanner (or (path pattern-uri) ""))))
    (and (or (not (port pattern-uri))
             (not (port uri))
             (= (port uri) (port pattern-uri)))
         (<= (length (domains pattern-uri)) (length (domains uri)))
         (loop for a in (domains uri)
               for b in (domains pattern-uri)
               always (string-equal a b))
         (not (null (cl-ppcre:scan (matcher pattern-uri) (or (path uri) "")))))))

(defun merge-uris (uri &optional (defaults *default-uri-defaults*))
  (let ((uri (ensure-uri uri))
        (defaults (ensure-uri defaults)))
    (make-uri
     :domains (append (domains uri) (domains defaults))
     :port (or (port uri) (port defaults))
     :path (format NIL "~@[~a/~]~@[~a~]"
                   (or* (path defaults)) (path uri)))))

(defun represent-uri (uri representation)
  (ecase representation
    ((:as-is NIL) (copy-uri uri))
    ((:external) (external-uri uri))
    ((:internal) (internal-uri uri))))

(defun uri-to-url (uri &key (representation :as-is))
  (let* ((uri (represent-uri uri representation))
         (proto (case (port uri)
                  ((443) "https")
                  ((80 NIL) "http")
                  (T "http")))
         (port (case (port uri)
                 ((443 80) NIL)
                 (T (port uri)))))
    (format NIL "~a://~{~a~^.~}~@[:~a~]/~a"
            proto (reverse (domains uri)) port (or (path uri) ""))))
