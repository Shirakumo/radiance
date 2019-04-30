#|
This file is a part of Radiance
(c) 2019 Shirakumo https://shirakumo.org/ (shinmera@tymoon.eu)
Author: Harald Judt <h.judt@gmx.at>
|#
(in-package #:org.shirakumo.radiance.test)

(defparameter *requests* nil
  "Holds requests sent to radiance by the test suite. These happen
sequentially, so no need to protect it against multiple simultaneous
accesses.")

(defparameter *body-streams* nil
  "Holds processed body-streams of *requests*. It is necessary to keep
them separately because the body-streams are only available during the
API endpoint processing, and will have been already closed when
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
      (loop with uri = (uiop:strcat *get-uri* "?")
            with count = (length get-params)
            for pos from 1
            for (key . val) in get-params do
              (setq uri (uiop:strcat uri key "=" val))
            while (< pos count) do
              (setq uri (uiop:strcat uri "&"))
            finally (return uri))
      *get-uri*))

(defun ht-entry-equals (hash-table key val)
  "Checks whether key in hash-table exists and its value has been set
to val."
    (multiple-value-bind (value found)
        (gethash key hash-table)
      (when found
        (string= val value))))

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
  (dex:get (get-uri) :use-connection-pool nil :keep-alive nil)
  (dex:get (get-uri '(("user" . "john_doe")))
           :use-connection-pool nil :keep-alive nil)
  (dex:get (get-uri '(("user" . "john_doe")("mail" . "abc@hotmail.com")))
           :use-connection-pool nil :keep-alive nil)
  (destructuring-bind (req1 req2 req3)
      (nreverse *requests*)
    (of-type radiance:request req1)
    (of-type radiance:request req2)
    (of-type radiance:request req3)
    (is equal :get (radiance:http-method req1))
    (is equal :get (radiance:http-method req2))
    (is equal :get (radiance:http-method req3))
    (true (stringp (radiance:domain req1)))
    (true (stringp (radiance:domain req2)))
    (true (stringp (radiance:domain req3)))
    (true (integerp (radiance:issue-time req1)))
    (true (integerp (radiance:issue-time req2)))
    (true (integerp (radiance:issue-time req3)))
    (is > 0 (radiance:issue-time req1))
    (is > 0 (radiance:issue-time req2))
    (is > 0 (radiance:issue-time req3))
    (true (remote-addr-p (radiance:remote req1)))
    (true (remote-addr-p (radiance:remote req2)))
    (true (remote-addr-p (radiance:remote req3)))
    (of-type radiance:uri (radiance:uri req1))
    (of-type radiance:uri (radiance:uri req2))
    (of-type radiance:uri (radiance:uri req3))
    (true (string= (radiance:user-agent req1) dexador.util:*default-user-agent*))
    (true (string= (radiance:user-agent req2) dexador.util:*default-user-agent*))
    (true (string= (radiance:user-agent req3) dexador.util:*default-user-agent*))
    (false (radiance:header "content-type" req1) "No content type set.")
    (false (radiance:header "content-type" req2) "No content type set.")
    (false (radiance:header "content-type" req3) "No content type set.")
    (destructuring-bind (get1 get2 get3)
        (list (radiance:get-data req1)
              (radiance:get-data req2)
              (radiance:get-data req3))
      (is equal (hash-table-count get1) 0)
      (is equal (hash-table-count get2) 1)
      (is equal (hash-table-count get3) 2)
      (true (ht-entry-equals get2 "user" "john_doe"))
      (true (ht-entry-equals get3 "user" "john_doe"))
      (true (ht-entry-equals get3 "mail" "abc@hotmail.com")))))

(define-test post-requests
  :parent requests
  (setf *requests* nil)
  (dex:post *post-uri* :use-connection-pool nil :keep-alive nil)
  (dex:post *post-uri* :content '(("user" . "john_doe"))
                       :use-connection-pool nil :keep-alive nil)
  (dex:post *post-uri* :content '(("user" . "john_doe")
                                  ("mail" . "abc@hotmail.com"))
                       :use-connection-pool nil :keep-alive nil)
  (destructuring-bind (req1 req2 req3)
      (nreverse *requests*)
    (of-type radiance:request req1)
    (of-type radiance:request req2)
    (of-type radiance:request req3)
    (is equal :post (radiance:http-method req1))
    (is equal :post (radiance:http-method req2))
    (is equal :post (radiance:http-method req3))
    (true (stringp (radiance:domain req1)))
    (true (stringp (radiance:domain req2)))
    (true (stringp (radiance:domain req3)))
    (true (integerp (radiance:issue-time req1)))
    (true (integerp (radiance:issue-time req2)))
    (true (integerp (radiance:issue-time req3)))
    (is > 0 (radiance:issue-time req1))
    (is > 0 (radiance:issue-time req2))
    (is > 0 (radiance:issue-time req3))
    (true (remote-addr-p (radiance:remote req1)))
    (true (remote-addr-p (radiance:remote req2)))
    (true (remote-addr-p (radiance:remote req3)))
    (of-type radiance:uri (radiance:uri req1))
    (of-type radiance:uri (radiance:uri req2))
    (of-type radiance:uri (radiance:uri req3))
    (true (string= (radiance:user-agent req1) dexador.util:*default-user-agent*))
    (true (string= (radiance:user-agent req2) dexador.util:*default-user-agent*))
    (true (string= (radiance:user-agent req3) dexador.util:*default-user-agent*))
    (false (radiance:header "content-type" req1) "No content type set.")
    (true (string= (radiance:header "content-type" req2)
                   "application/x-www-form-urlencoded"))
    (true (string= (radiance:header "content-type" req3)
                   "application/x-www-form-urlencoded"))
    (destructuring-bind (post1 post2 post3)
        (list (radiance:post-data req1)
              (radiance:post-data req2)
              (radiance:post-data req3))
      (is equal (hash-table-count post1) 0)
      (is equal (hash-table-count post2) 1)
      (is equal (hash-table-count post3) 2)
      (true (ht-entry-equals post2 "user" "john_doe"))
      (true (ht-entry-equals post3 "user" "john_doe"))
      (true (ht-entry-equals post3 "mail" "abc@hotmail.com")))))

(define-test raw-post-requests
  :parent requests
  :fix (*body-streams*)
  (setf *requests* nil)
  (setf *body-streams* nil)
  (let ((json "{ \"user\": \"john_doe\", \"mail:\" \"abc@hotmail.com\" }"))
    (dex:post *raw-uri*
              :headers '(("content-type" . "application/json"))
              :content json
              :use-connection-pool nil
              :keep-alive nil)
    (destructuring-bind (req1)
        (nreverse *requests*)
      (destructuring-bind (body1)
          (nreverse *body-streams*)
        (of-type radiance:request req1)
        (is equal :post (radiance:http-method req1))
        (true (stringp (radiance:domain req1)))
        (true (integerp (radiance:issue-time req1)))
        (is > 0 (radiance:issue-time req1))
        (true (remote-addr-p (radiance:remote req1)))
        (of-type radiance:uri (radiance:uri req1))
        (true (string= (radiance:user-agent req1) dexador.util:*default-user-agent*))
        (true (string= (radiance:header "content-type" req1)
                       "application/json"))
        (true (radiance:body-stream req1))
        (is equal (hash-table-count (radiance:post-data req1)) 0)
        (is equal json (flexi-streams:octets-to-string body1))))))
