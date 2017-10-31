#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.test)

(defun run-test (&key (report 'parachute:plain) (test 'radiance))
  (v:with-muffled-logging ()
    (let ((*package* #.*package*))
      (parachute:test (etypecase test
                        (symbol test)
                        (string (find-symbol test *package*)))
                      :report report))))

(define-test radiance
  (unless (radiance:started-p)
    (radiance:startup)))

(define-test interfaces
  :parent radiance)

(defun pathname~= (a b)
  (equal (namestring a) (namestring b)))

(define-test toolkit
  :parent radiance
  (is = 0 (radiance:universal-to-unix-time (encode-universal-time 0 0 0 1 1 1970 0)))
  (is = (encode-universal-time 0 0 0 1 1 1970 0) (radiance:unix-to-universal-time 0))
  (is equal "0 seconds" (radiance:format-relative-time 0))
  (is equal "1 second" (radiance:format-relative-time 1))
  (is equal "1 minute, 1 second" (radiance:format-relative-time 61))
  (is equal "0:00:00" (radiance:format-clock-time 0))
  (is equal "0:01:01" (radiance:format-clock-time 61))
  (is equal "1900-01-01T00:00:00" (radiance:format-machine-date 0))
  (is equal "1900.01.01 00:00:00" (radiance:format-human-date 0))
  (is equal "Monday, 1st of January 1900, 0:00:00 UTC" (radiance:format-fancy-date 0))
  ;; Not sure how to test format-time properly since it's current-time dependant.
  (is = 16 (length (radiance:make-random-string)))
  (is = 12 (length (radiance:make-random-string 12)))
  (is equal "aaaaaa" (radiance:make-random-string 6 "a"))
  ;; If this fails you're a very unlucky guy.
  (isnt equal (radiance:make-random-string 16) (radiance:make-random-string 16))
  (uiop:with-temporary-file (:stream s :pathname p :direction :io
                             :element-type '(unsigned-byte 8))
    (write-byte 0 s)
    (finish-output s)
    (is = 1 (radiance:file-size p)))
  (let ((pathname #p"~/test"))
    (is eq pathname (radiance:resolve-base pathname)))
  (fail (radiance:resolve-base '%foobar-package))
  (is pathname~= (asdf:system-source-directory :radiance-test) (radiance:resolve-base NIL))
  (is pathname~= (asdf:system-source-directory :radiance) (radiance:resolve-base :radiance-core))
  (is pathname~= (asdf:system-source-directory :radiance) (radiance:resolve-base :radiance))
  (is pathname~= (asdf:system-relative-pathname :radiance "static/foo") (radiance:static-file "foo" :radiance))
  (is pathname~= (asdf:system-relative-pathname :radiance-test "static/foo") (radiance:@static "foo"))
  (is pathname~= (asdf:system-relative-pathname :radiance "template/foo") (radiance:template-file "foo" :radiance))
  (is pathname~= (asdf:system-relative-pathname :radiance-test "template/foo") (radiance:@template "foo"))
  (is = 1 (radiance:or* 1 2 3))
  (is = 1 (radiance:or* NIL 1 2 3))
  (is = 1 (radiance:or* NIL "" 1 2 3))
  (is equal "1" (radiance:or* "" "1" "2" "3"))
  (let ((a 0))
    (is = 1 (radiance:or* (incf a) (incf a))))
  (is equal '(a b c) (radiance:perm a b c))
  (is equal "foo" (pathname-name (radiance:parse-path-safely "foo")))
  (is equal "foo" (pathname-name (radiance:parse-path-safely "foo.bar")))
  (is equal "bar" (pathname-type (radiance:parse-path-safely "foo.bar")))
  (is equal "foo" (pathname-name (radiance:parse-path-safely "baz/foo.bar")))
  (is equal "bar" (pathname-type (radiance:parse-path-safely "baz/foo.bar")))
  (is equal "baz" (second (pathname-directory (radiance:parse-path-safely "baz/foo.bar"))))
  (is eql :relative (first (pathname-directory (radiance:parse-path-safely "baz/foo.bar"))))
  (is eql :relative (first (pathname-directory (radiance:parse-path-safely "/baz/foo.bar"))))
  (is = 2 (length (pathname-directory (radiance:parse-path-safely "baz/foo.bar"))))
  (is = 2 (length (pathname-directory (radiance:parse-path-safely "baz//foo.bar"))))
  (is = 3 (length (pathname-directory (radiance:parse-path-safely "baz/foo/bar"))))
  (is equal "a" (radiance:url-encode "a"))
  (is equal "%25" (radiance:url-encode "%"))
  (is equal "%" (radiance:url-encode "%" :allowed "%"))
  (is equal "%E5%A4%9C" (radiance:url-encode "夜" :external-format :utf-8))
  (is equal "%FF%FE%1CY" (radiance:url-encode "夜" :external-format :utf-16))
  (fail (radiance:url-encode "夜" :external-format :ascii))
  ;; FIXME: test url-encode to stream
  ;; FIXME: test with-actions
  )
