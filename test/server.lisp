(in-package #:org.shirakumo.radiance.test)

(defparameter *requests* nil
  "Holds requests sent to radiance by the test suite. These happen
sequentially, so no need to protect it against multiple simultaneous
accesses.")

(defparameter *body-streams* nil
  "Holds bytes processed from body-streams of *requests*. It is necessary
to keep them separately because the body-streams are only available during
the API endpoint processing, and will have been already closed when
testing.")

(defparameter *get-uri* "http://localhost:8080/api/test/request/get")

(defparameter *post-uri* "http://localhost:8080/api/test/request/post")

(defparameter *raw-uri* "http://localhost:8080/api/test/request/raw")

(radiance:define-api test/request/get () ()
  "API endpoint to send test requests to for later analysis."
  (push (radiance:*request*) *requests*)
  (radiance:api-output "GET request received."))

(radiance:define-api test/request/post () ()
  "API endpoint to send POST test requests to for later analysis."
  (push (radiance:*request*) *requests*)
  (radiance:api-output "POST request received."))

(radiance:define-api test/request/raw () ()
  "API endpoint to send POST test requests to for later analysis of
their body-streams."
  (let ((request (radiance:*request*)))
    (push request *requests*)
    (push
     (loop with stream = (radiance:body-stream request)
           for byte = (read-byte stream nil)
           while byte collect byte)
     *body-streams*))
  (radiance:api-output "POST request with body-stream received."))

(defun get-uri (&optional get-params)
  "Make a URI from get-params, which need to be provided as an alist."
  (if get-params
      (format NIL "~a?~/radiance-core::format-query/" *get-uri* get-params)
      *get-uri*))

(defun remote-addr-p (addr)
  "Tests whether addr is a remote IP address."
  (cond ((not (stringp addr)) nil)
        ((string= addr "::1") t)
        ((string= addr "127.0.0.1") t)
        ((cl-ppcre:scan-to-strings "^\\d+\\.\\d+\\.\\d+\\.\\d+$" addr) t)
        ((cl-ppcre:scan-to-strings "^[0-9abcdef:]+$" addr) t)
        (t nil)))

(define-test requests
  :parent radiance
  :fix (*requests*))

(define-test get-requests
  :parent requests
  (setf *requests* nil)
  (dex:get (get-uri) :use-connection-pool nil :keep-alive nil
                     :headers '(("User-Agent" . "test-suite")))
  (dex:get (get-uri '(("user" . "john_doe")))
           :use-connection-pool nil :keep-alive nil
           :headers '(("User-Agent" . "test-suite")))
  (dex:get (get-uri '(("user" . "john_doe")("mail" . "abc@hotmail.com")))
           :use-connection-pool nil :keep-alive nil
           :headers '(("User-Agent" . "test-suite")))
  (destructuring-bind (req1 req2 req3)
      (nreverse *requests*)
    (of-type radiance:request req1)
    (of-type radiance:request req2)
    (of-type radiance:request req3)
    (is equal :get (radiance:http-method req1))
    (is equal :get (radiance:http-method req2))
    (is equal :get (radiance:http-method req3))
    (of-type string (radiance:domain req1))
    (of-type string (radiance:domain req2))
    (of-type string (radiance:domain req3))
    (of-type integer (radiance:issue-time req1))
    (of-type integer (radiance:issue-time req2))
    (of-type integer (radiance:issue-time req3))
    (is > 0 (radiance:issue-time req1))
    (is > 0 (radiance:issue-time req2))
    (is > 0 (radiance:issue-time req3))
    (true (remote-addr-p (radiance:remote req1)))
    (true (remote-addr-p (radiance:remote req2)))
    (true (remote-addr-p (radiance:remote req3)))
    (of-type radiance:uri (radiance:uri req1))
    (of-type radiance:uri (radiance:uri req2))
    (of-type radiance:uri (radiance:uri req3))
    (is string= "test-suite" (radiance:user-agent req1))
    (is string= "test-suite" (radiance:user-agent req2))
    (is string= "test-suite" (radiance:user-agent req3))
    (false (radiance:header "content-type" req1) "No content type set.")
    (false (radiance:header "content-type" req2) "No content type set.")
    (false (radiance:header "content-type" req3) "No content type set.")
    (let ((get1 (radiance:get-data req1))
          (get2 (radiance:get-data req2))
          (get3 (radiance:get-data req3)))
      (is = 0 (hash-table-count get1))
      (is = 1 (hash-table-count get2))
      (is = 2 (hash-table-count get3))
      (is string= "john_doe" (gethash "user" get2))
      (is string= "john_doe" (gethash "user" get3))
      (is string= "abc@hotmail.com" (gethash "mail" get3)))))

(define-test post-requests
  :parent requests
  (setf *requests* nil)
  (dex:post *post-uri* :use-connection-pool nil :keep-alive nil
                       :headers '(("User-Agent" . "test-suite")))
  (dex:post *post-uri* :content '(("user" . "john_doe"))
                       :use-connection-pool nil :keep-alive nil
                       :headers '(("User-Agent" . "test-suite")))
  (dex:post *post-uri* :content '(("user" . "john_doe")
                                  ("mail" . "abc@hotmail.com"))
                       :use-connection-pool nil :keep-alive nil
                       :headers '(("User-Agent" . "test-suite")))
  (destructuring-bind (req1 req2 req3)
      (nreverse *requests*)
    (of-type radiance:request req1)
    (of-type radiance:request req2)
    (of-type radiance:request req3)
    (is eql :post (radiance:http-method req1))
    (is eql :post (radiance:http-method req2))
    (is eql :post (radiance:http-method req3))
    (of-type string (radiance:domain req1))
    (of-type string (radiance:domain req2))
    (of-type string (radiance:domain req3))
    (of-type integer (radiance:issue-time req1))
    (of-type integer (radiance:issue-time req2))
    (of-type integer (radiance:issue-time req3))
    (is > 0 (radiance:issue-time req1))
    (is > 0 (radiance:issue-time req2))
    (is > 0 (radiance:issue-time req3))
    (true (remote-addr-p (radiance:remote req1)))
    (true (remote-addr-p (radiance:remote req2)))
    (true (remote-addr-p (radiance:remote req3)))
    (of-type radiance:uri (radiance:uri req1))
    (of-type radiance:uri (radiance:uri req2))
    (of-type radiance:uri (radiance:uri req3))
    (is string= "test-suite" (radiance:user-agent req1))
    (is string= "test-suite" (radiance:user-agent req2))
    (is string= "test-suite" (radiance:user-agent req3))
    (is eql NIL (radiance:header "content-type" req1) "No content type set.")
    (is string= "application/x-www-form-urlencoded" (radiance:header "content-type" req2))
    (is string= "application/x-www-form-urlencoded" (radiance:header "content-type" req3))
    (let ((post1 (radiance:post-data req1))
          (post2 (radiance:post-data req2))
          (post3 (radiance:post-data req3)))
      (is = 0 (hash-table-count post1))
      (is = 1 (hash-table-count post2))
      (is = 2 (hash-table-count post3))
      (is string= "john_doe" (gethash "user" post2))
      (is string= "john_doe" (gethash "user" post3))
      (is string= "abc@hotmail.com" (gethash "mail" post3)))))

(define-test raw-post-requests
  :parent requests
  :fix (*body-streams*)
  (setf *requests* nil)
  (setf *body-streams* nil)
  (let ((json "{ \"user\": \"john_doe\", \"mail\": \"abc@hotmail.com\" }"))
    (dex:post *raw-uri*
              :headers '(("content-type" . "application/json")
                         ("User-Agent" . "test-suite"))
              :content json
              :use-connection-pool nil
              :keep-alive nil)
    (destructuring-bind (req1)
        (nreverse *requests*)
      (of-type radiance:request req1)
      (is eql :post (radiance:http-method req1))
      (of-type string (radiance:domain req1))
      (of-type integer (radiance:issue-time req1))
      (is > 0 (radiance:issue-time req1))
      (true (remote-addr-p (radiance:remote req1)))
      (of-type radiance:uri (radiance:uri req1))
      (is string= "test-suite" (radiance:user-agent req1))
      (is string= "application/json" (radiance:header "content-type" req1))
      (true (radiance:body-stream req1))
      (is equal (hash-table-count (radiance:post-data req1)) 0)
      (let ((body1 (first *body-streams*)))
        (is equal json (flexi-streams:octets-to-string body1))))))
