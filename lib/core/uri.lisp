#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass uri ()
  ((subdomains :initarg :subdomains :initform NIL :accessor subdomains :type list)
   (domain :initarg :domain :initform NIL :accessor domain :type (or string null))
   (port :initarg :port :initform NIL :accessor port :type (or fixnum null))
   (path :initarg :path :initform ".*" :accessor path :type string)
   (pathregex :initarg :regex :initform NIL :accessor regex))
  (:documentation "URI class used in Radiance to build links and such."))

(defmethod print-object ((uri uri) stream)
  (with-slots (subdomains domain port path) uri
    (format stream "~:[*~;~:*~{~a~^.~}~].~:[*~;~:*~a~]:~:[*~;~:*~a~]/~a" subdomains domain port path)))

(defgeneric uri-matches (uri b) (:documentation "Checks if a URI matches."))

(defmethod uri-matches ((uri uri) (string string))
  "Checks if the given URI is compatible with the string representation of a URI."
  (uri-matches uri (make-uri string)))

(defmethod uri-matches ((uri uri) (uri2 uri))
  "Checks if the given URI is compatible with the other URI."
  (declare (optimize (speed 3)))
  (and (or (not (domain uri))
           (not (domain uri2))
           (equal (domain uri) (domain uri2)))
       (or (not (port uri))
           (not (port uri2))
           (equal (port uri) (port uri)))
       (or (not (subdomains uri))
           (not (subdomains uri2))
           (loop for sda in (reverse (subdomains uri))
              for sdb in (reverse (subdomains uri2))
              unless (string= sda sdb)
              return NIL
              finally (return T)))
       (or (not (path uri))
           (not (path uri2))
           (cl-ppcre:scan (regex uri) (path uri2)))))

(defun uri-same (uri uri2)
  "Checks if the given URIs are identical."
  (declare (optimize (speed 3)))
  (string-equal (format NIL "~a" uri) (format NIL "~a" uri2)))


(defun uri->url (uri &optional (absolute T))
  "Turns the URI into a string URL."
  (if absolute 
      (format NIL "http://~{~a.~}~a~:[~;:~:*~a~]/~a"
              (subdomains uri) (domain uri) (port uri) (path uri))
      (concatenate 'string "/" (path uri))))

(defun uri->server-url (uri)
  (format NIL "http://~{~a.~}~:[~a~;~:*~a~*~]~:[:~a~;:~:*~a~*~]/~a"
          (subdomains uri) (domain uri) (config-tree :domain) (port uri) (first (config-tree :ports)) (path uri)))

(defun uri->context-url (uri)
  (format NIL "http://~:[~{~a.~}~;~:*~{~a.~}~*~]~:[~a~;~:*~a~*~]~:[:~a~;:~:*~a~*~]/~a"
          (subdomains uri) (subdomains *radiance-request*) (domain uri) (domain *radiance-request*) (port uri) (port *radiance-request*) (path uri)))

(defun make-uri (uristring)
  "Creates a URI object matching the urispec. Urispec has the following
syntax:  (subdomain.)*domain?:port?/path?

If a part of the URI is not given, it is defaulted to \"*\", which
matches to anything. make-uri has a read-macro for easier use: #u
Note that the PATH part is always a regex, excluding the start slash."
  (cl-ppcre:register-groups-bind (subdomains NIL domain NIL port path) (*uri-matcher* uristring)
    (setf path (if (= (length path) 0) ".*" path))
    (setf subdomains (if (= (length subdomains) 0) NIL (split-sequence:split-sequence #\. (string-trim "." subdomains))))
    (make-instance 'uri 
                   :path path
                   :subdomains subdomains
                   :port port :domain domain
                   :regex (cl-ppcre:create-scanner path))))

(defun %make-uri (stream subchar arg)
  (declare (ignore subchar arg))
  (let ((string (read stream T)))
    `(make-uri ,string)))

(set-dispatch-macro-character #\# #\u #'%make-uri)
