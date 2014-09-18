#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

;; Extend SQLite for loading capability
(cffi:defcfun sqlite3-enable-load-extension :int
  (db sqlite-ffi:p-sqlite3)
  (onoff :int))

(defun sqlite::load-extension (extensionpath &optional (connection *current-con*))
  (sqlite3-enable-load-extension (sqlite::handle connection) 1)
  (sqlite:execute-non-query connection (format NIL "SELECT load_extension('~a');"
					       (etypecase extensionpath
						 (pathname (uiop:native-namestring extensionpath))
						 (string extensionpath))))
  (sqlite3-enable-load-extension (sqlite::handle connection) 0)
  extensionpath)

;; We only really do this for PCRE.
(define-condition pcre-not-found (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "Could not find the sqlite3 pcre extension library. Please adapt I-SQLITE:*SQLITE-PCRE-PATHS*"))))

(defvar *sqlite-pcre-paths*
  (list #+unix #p"/usr/lib/sqlite3/pcre.so"
	#+unix #p"/usr/lib/sqlite3/pcre"
	#+unix #p"/usr/local/lib/sqlite3/pcre.so"
	#+unix #p"/usr/local/lib/sqlite3/pcre"
	#+unix (data-file "sqlite3-pcre.so")
	#+unix "pcre.so"
	#+windows #p"C:/Windows/System32/pcre.dll"
	#+windows (data-file "sqlite3-pcre.dll")
	#+windows "pcre.dll"))

(defun load-pcre ()
  (or
   (loop for path in *sqlite-pcre-paths*
	 thereis (when (probe-file path)
		   (sqlite::load-extension path)))
   (error 'pcre-not-found)))
