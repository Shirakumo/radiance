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

(defparameter *test-uri* "http://localhost:8080/api/test/request")

(radiance:define-api test/request () ()
  "API endpoint to send test requests to for later analysis."
  (push radiance:*request* *requests*)
  (radiance:api-output nil))

(defun get-uri (&optional get-params)
  "Make a URI from get-params, which need to be provided as an alist."
  (if get-params
      (loop with uri = (uiop:strcat *test-uri* "?")
            with count = (length get-params)
            for pos from 1
            for (key . val) in get-params do
              (setq uri (uiop:strcat uri key "=" val))
            while (< pos count) do
              (setq uri (uiop:strcat uri "&"))
            finally (return uri))
      *test-uri*))

(defun ht-entry-equals (hash-table key val)
  "Checks whether key in hash-table exists and its value has been set
to val."
    (multiple-value-bind (value found)
        (gethash key hash-table)
      (when found
        (string= val value))))

(define-test requests
  :parent radiance
  :fix (*requests*))

(define-test get-requests
  :parent requests
  (setf *requests* nil)
  (dex:get (get-uri))
  (dex:get (get-uri '(("user" . "john_doe"))))
  (dex:get (get-uri '(("user" . "john_doe")("mail" . "abc@hotmail.com"))))
  (destructuring-bind (req1 req2 req3)
      (nreverse *requests*)
    (of-type radiance:request req1)
    (of-type radiance:request req2)
    (of-type radiance:request req3)
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
  (dex:post *test-uri*)
  (dex:post *test-uri* :content '(("user" . "john_doe")))
  (dex:post *test-uri* :content '(("user" . "john_doe")
                                  ("mail" . "abc@hotmail.com")))
  (destructuring-bind (req1 req2 req3)
      (nreverse *requests*)
    (of-type radiance:request req1)
    (of-type radiance:request req2)
    (of-type radiance:request req3)
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
