#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:i-sqlite)

;; Add our data path to the lib search function so people
;; can put the dlls or whatnots in there.
(push *data-path* cffi:*foreign-library-directories*)

;; Extend SQLite for loading capability
(cffi:defcfun sqlite3-enable-load-extension :int
  (db sqlite-ffi:p-sqlite3)
  (onoff :int))

(defun read-new-value ()
  (format T "Enter a new path to use (evaluated, pathname or string): ")
  (eval (read)))

(defun sqlite::load-extension (extensionpath &optional (connection *current-con*))
  (loop until (restart-case
                  (progn
                    (sqlite3-enable-load-extension (sqlite::handle connection) 1)
                    (sqlite:execute-non-query connection (format NIL "SELECT load_extension('~a');"
                                                                 (etypecase extensionpath
                                                                   (pathname (uiop:native-namestring extensionpath))
                                                                   (string extensionpath))))
                    (sqlite3-enable-load-extension (sqlite::handle connection) 0)
                    extensionpath)
                (use-path (new-path)
                  :report "Use a different path."
                  :interactive read-new-value
                  (setf extensionpath new-path)
                  NIL))))

;; We only really do this for PCRE.
(define-condition pcre-not-found (error) ()
  (:report (lambda (c s) (declare (ignore c)) (format s "Could not find the sqlite3 pcre extension library. Please adapt I-SQLITE:*SQLITE-PCRE-PATHS*"))))

(defvar *sqlite-pcre-paths*
  (append
   (loop for name in '("sqlite3-pcre" "pcre3" "pcre")
         for fqn = (format NIL "~a.~a" name #+windows "dll"
                                            #+unix "so")
         appending (list name fqn (data-file fqn)))
   (list #+unix #p"/usr/lib/sqlite3/pcre.so"
         #+unix #p"/usr/local/lib/sqlite3/pcre.so"
         #+windows #p"C:/Windows/System32/sqlite3-pcre.dll"
         #+windows #p"C:/Windows/System32/pcre.dll")))

(defun load-pcre ()
  (with-simple-restart (ignore "Ignore the fact that SQLite won't have PCRE support and pray that all goes well anyway.")
    (or
     (loop for path in *sqlite-pcre-paths*
           thereis (when (probe-file path)
                     (sqlite::load-extension path)))
     (error 'pcre-not-found))))
