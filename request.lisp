(in-package #:org.shirakumo.radiance.core)

(defvar *request*)
(defvar *response*)
(defvar *default-external-format* :UTF-8)
(defvar *default-content-type* "text/html")

(defun *request* () *request*)
(defun *response* () *response*)

(defclass request ()
  ((uri :initarg :uri :accessor uri)
   (http-method :initarg :http-method :accessor http-method)
   (body-stream :initarg :body-stream :accessor body-stream)
   (headers :initarg :headers :accessor headers)
   (post-data :initarg :post-data :accessor post-data)
   (get-data :initarg :get-data :accessor get-data)
   (cookies :initarg :cookies :accessor cookies)
   (domain :initarg :domain :accessor domain)
   (remote :initarg :remote :accessor remote)
   (data :initarg :data :accessor data)
   (issue-time :initarg :issue-time :accessor issue-time))
  (:default-initargs
   :uri (error "URI required")
   :http-method :get
   :body-stream NIL
   :headers (make-hash-table :test 'equalp)
   :post-data (make-hash-table :test 'equalp)
   :get-data (make-hash-table :test 'equalp)
   :cookies (make-hash-table :test 'equalp)
   :domain "localhost"
   :remote "unknown"
   :data (make-hash-table :test 'eql)
   :issue-time (get-universal-time)))

(defmethod print-object ((request request) stream)
  (print-unreadable-object (request stream :type T)
    (format stream "~a ~s" (http-method request) (uri request)))
  request)

(defclass response ()
  ((data :initarg :data :accessor data)
   (return-code :initarg :return-code :accessor return-code)
   (content-type :initarg :content-type :accessor content-type)
   (external-format :initarg :external-format :accessor external-format)
   (headers :initarg :headers :accessor headers)
   (cookies :initarg :cookies :accessor cookies))
  (:default-initargs
   :data NIL
   :return-code 200
   :content-type *default-content-type*
   :external-format *default-external-format*
   :headers (make-hash-table :test 'equalp)
   :cookies (make-hash-table :test 'equalp)))

(defmethod print-object ((response response) stream)
  (print-unreadable-object (response stream :type T)
    (format stream "~d ~s" (return-code response) (content-type response)))
  response)

(defclass cookie ()
  ((name :initarg :name :accessor name)
   (value :initarg :value :accessor value)
   (domain :initarg :domain :accessor domain)
   (path :initarg :path :accessor path)
   (expires :initarg :expires :accessor expires)
   (http-only :initarg :http-only :accessor http-only)
   (secure :initarg :secure :accessor secure))
  (:default-initargs
   :name (error "NAME required")
   :value ""
   :domain NIL
   :path NIL
   :expires NIL
   :http-only NIL
   :secure NIL))

(defun %cookie-time (stream time atp cp)
  (declare (ignore atp cp))
  (local-time:format-timestring
   stream (local-time:universal-to-timestamp time)
   :format local-time:+rfc-1123-format+))

(defun cookie-header (cookie)
  (format NIL "Set-Cookie: ~a=~a;~@[Domain=~a;~]~@[Path=~a;~]~@[Secure;~*~]~@[HttpOnly;~*~]~@[Expires=~/radiance::%cookie-time/;~]"
          (name cookie) (value cookie) (domain cookie) (path cookie) (secure cookie) (http-only cookie) (expires cookie)))

(defmethod print-object ((c cookie) stream)
  (print-unreadable-object (c stream :type T)
    (format stream "~a=~s ~@[~a~]~@[~a~] (~:[SESSION~;~:*~a~])~:[~; HTTP-ONLY~]~:[~; SECURE~]"
            (name c) (value c) (domain c) (path c) (expires c) (http-only c) (secure c))))

(defun user-agent (&optional (request *request*))
  (gethash "user-agent" (headers request)))

(defun (setf user-agent) (value &optional (request *request*))
  (setf (gethash "user-agent" (headers request)) value))

(defun referer (&optional (request *request*))
  (gethash "referer" (headers request)))

(defun (setf referer) (value &optional (request *request*))
  (setf (gethash "referer" (headers request)) value))

(defun cookie (name &optional (request/response *request*))
  (gethash (string name) (cookies request/response)))

(defun (setf cookie) (value name &key domain path timeout http-only secure (response *response*))
  (setf (gethash name (cookies response))
        (make-instance 'cookie :name name :value value :domain domain :path path :expires timeout :http-only http-only :secure secure)))

(defun get-var (name &optional (request *request*))
  (gethash (string name) (get-data request)))

(defun post-var (name &optional (request *request*))
  (gethash (string name) (post-data request)))

(defun post/get (name &optional (request *request*))
  (or (gethash (string name) (post-data request))
      (gethash (string name) (get-data request))))

(defun header (name &optional (request/response *request*))
  (gethash (string name) (headers request/response)))

(defun (setf header) (value name &optional (response *response*))
  (setf (gethash name (headers response)) value))

(defmethod content-type ((request request))
  (header "content-type"))

(defmethod content-length ((request request))
  (let ((header (header "content-length")))
    (when (and header (< 0 (length header)))
      (ignore-errors (parse-integer header)))))

(defun file (name &optional (request *request*))
  "Returns file info about a form uploaded file.
 (PATH ORIGINAL-FILENAME MIME-TYPE)"
  (let ((var (post-var name request)))
    (cond
      ((null var) (error 'no-such-post-parameter :request request :parameter name))
      ((listp var) var)
      (T (error 'post-parameter-not-a-file :request request :parameter name)))))

(defun redirect (new-address &optional (representation :external) (code 307) (response *response*))
  (setf (return-code response) code)
  (setf (header "Location" response)
        (etypecase new-address
          (string new-address)
          (uri (uri-to-url new-address :representation representation)))))

(defun serve-file (pathname &optional content-type (response *response*))
  (unless (probe-file pathname)
    (error 'file-to-serve-does-not-exist :file pathname))
  (setf (content-type response) (or content-type (mimes:mime-lookup pathname) "application/octet-stream"))
  (setf (data response) pathname))

(defun request-run-time (&optional (request *request*))
  (- (get-universal-time)
     (issue-time request)))
