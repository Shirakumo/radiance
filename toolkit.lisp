#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *random-string-characters* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789")
(defconstant +unix-epoch-difference+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun enlist (var &rest args)
  (if (listp var) var (list* var args)))

(declaim (inline universal-to-unix-time))
(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(declaim (inline unix-to-universal-time))
(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(declaim (inline get-unix-time))
(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun format-relative-time (stamp)
  (when (typep stamp 'local-time:timestamp)
    (setf stamp (local-time:timestamp-to-universal stamp)))
  (if (= stamp 0)
      (format NIL "0 seconds")
      (let ((seconds   (mod (floor (/ stamp 1)) 60))
            (minutes   (mod (floor (/ stamp 60)) 60))
            (hours     (mod (floor (/ stamp 60 60)) 24))
            (days      (mod (floor (/ stamp 60 60 24)) 7))
            ;; We approximate by saying each month has four weeks
            (weeks     (mod (floor (/ stamp 60 60 24 7)) 4))
            (months    (mod (floor (/ stamp 60 60 24 7 4)) 12))
            ;; More accurate through stamp in a year
            (years     (mod (floor (/ stamp 31557600)) 10))
            (decades   (mod (floor (/ stamp 31557600 10)) 10))
            (centuries (mod (floor (/ stamp 31557600 10 10)) (expt 10 (- 9 2))))
            (aeons          (floor (/ stamp 31557600 10 10 (expt 10 (- 9 2)))))
            (non-NIL ()))
        (flet ((p (i format) (when (< 0 i) (push (format NIL format i) non-NIL))))
          (p seconds "~a second~:p")
          (p minutes "~a minute~:p")
          (p hours "~a hour~:p")
          (p days "~a day~:p")
          (p weeks "~a week~:p")
          (p months "~a month~:p")
          (p years "~a year~:p")
          (p decades "~a decade~:p")
          (p centuries "~a centur~:@p")
          (p aeons "~a æon~:p")
          (format NIL "~{~a~^, ~}" non-NIL)))))

(defun format-clock-time (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   NIL stamp :format '(:hour ":" (:min 2) ":" (:sec 2))
             :timezone local-time:+utc-zone+))

(defun format-machine-date (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   NIL stamp :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2))
             :timezone local-time:+utc-zone+))

(defun format-human-date (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   NIL stamp :format '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))
             :timezone local-time:+utc-zone+))

(defun format-fancy-date (stamp)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   NIL stamp :format '(:long-weekday ", " :ordinal-day " of " :long-month " " :year ", " :hour ":" (:min 2) ":" (:sec 2) " UTC")
             :timezone local-time:+utc-zone+))

(defun format-time (stamp &optional (relative-time-threshold (* 60 60 24)))
  (when (typep stamp 'local-time:timestamp)
    (setf stamp (local-time:timestamp-to-universal stamp)))
  (let ((now (get-universal-time)))
    (cond ((and (< (- now relative-time-threshold) stamp) (<= stamp now))
           (format NIL "~a ago" (format-relative-time (- now stamp))))
          ((and (< stamp (+ now relative-time-threshold)) (< now stamp))
           (format NIL "in ~a" (format-relative-time (- stamp now))))
          (T
           (format NIL "at ~a" (format-human-date stamp))))))

(defun make-random-string (&optional (length 16) (chars *random-string-characters*))
  (loop with string = (make-array length :element-type 'character)
        with charlength = (length chars)
        for i from 0 below length
        do (setf (aref string i) (aref chars (random charlength)))
        finally (return string)))

(defun file-size (pathspec)
  (with-open-file (in pathspec :direction :input :element-type '(unsigned-byte 8))
    (file-length in)))

(defun resolve-base (thing)
  (etypecase thing
    (pathname thing)
    (null (resolve-base *package*))
    ((or string symbol package)
     (asdf:system-source-directory
      (modularize:virtual-module
       (modularize:module-identifier thing))))))

(defun read-value ()
  (eval (read *query-io*)))

(defmacro or* (&rest vals)
  (let ((arg (gensym "ARG")))
    `(or ,@(loop for val in vals
                 collect `(let ((,arg ,val))
                            (if (stringp ,arg)
                                (unless (string= ,arg "") ,arg)
                                ,arg))))))

(defun cut-get-part (url)
  (subseq url 0 (position #\? url)))

(defun static-file (namestring &optional base)
  (merge-pathnames namestring (merge-pathnames "static/" (merge-pathnames (resolve-base base)))))

(defmacro @static (&environment env namestring)
  (if (constantp namestring env)
      `(load-time-value (static-file ,namestring ,*package*))
      `(static-file ,namestring ,*package*)))

(defun template-file (namestring &optional base)
  (merge-pathnames namestring (merge-pathnames "template/" (merge-pathnames (resolve-base base)))))

(defmacro @template (&environment env namestring)
  (if (constantp namestring env)
      `(load-time-value (template-file ,namestring ,*package*))
      `(template-file ,namestring ,*package*)))

(defmacro perm (&rest tree)
  (let ((perm (format NIL "~{~(~a~)~^.~}" tree)))
    ;; Execute during compile
    (pushnew perm (module-permissions (module)) :test #'string=)
    ;; And ensure during load as well.
    `(load-time-value
      (progn
        (pushnew ,perm (module-permissions ,(module)) :test #'string=)
        ,perm))))

(defun copy-hash-table (table &key (test (hash-table-test table))
                                   (size (hash-table-size table))
                                   (rehash-size (hash-table-rehash-size table))
                                   (rehash-threshold (hash-table-rehash-threshold table)))
  (let ((new (make-hash-table :test test :size size :rehash-size rehash-size :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v) (setf (gethash k new) v)) table)
    new))

(defun parse-path-safely (namestring)
  (let ((name NIL)
        (type NIL)
        (path ()))
    (flet ((process-segment (string)
             (unless (string= string "")
               (push string path)))
           (process-filename (string)
             (unless (string= string "")
               (let ((endpos (position #\. string :from-end T)))
                 (cond (endpos
                        (setf name (subseq string 0 endpos))
                        (setf type (subseq string (1+ endpos))))
                       (T (setf name string)))))))
      (loop with buf = (make-string-output-stream)
            for char across namestring
            do (case char
                 (#\/ (process-segment (get-output-stream-string buf)))
                 (T (write-char char buf)))
            finally (process-filename (get-output-stream-string buf))))
    (make-pathname :name name :type type :directory `(:relative ,@(nreverse path)))))

(defun url-encode (thing &key (stream NIL) (external-format *default-external-format*) (allowed "-._~"))
  (flet ((%url-encode (stream)
           (loop for octet across (babel:string-to-octets thing :encoding external-format)
                 for char = (code-char octet)
                 do (cond ((or (char<= #\0 char #\9)
                               (char<= #\a char #\z)
                               (char<= #\A char #\Z)
                               (find char allowed :test #'char=))
                           (write-char char stream))
                          (T (format stream "%~2,'0x" (char-code char)))))))
    (if stream
        (%url-encode stream)
        (with-output-to-string (stream)
          (%url-encode stream)))))

(defun format-query (stream arg &rest args)
  (declare (ignore args))
  (loop for cons on arg
        for (key . val) = (car cons)
        do (url-encode key :stream stream)
           (write-char #\= stream)
           (url-encode val :stream stream)
           (when (cdr cons)
             (write-char #\& stream))))

(defun format-urlpart (stream arg &rest args)
  (declare (ignore args))
  (url-encode arg :stream stream :allowed "-._~!$&()*+,;=:@/"))

(defun ecompile (name &optional definition)
  (multiple-value-bind (function warnings-p failure-p) (compile name definition)
    (if failure-p
        (error "Compilation of ~a with definition~%  ~s~%failed." name definition)
        (values function warnings-p failure-p))))
