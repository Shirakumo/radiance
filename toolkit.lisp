(in-package #:org.shirakumo.radiance.core)

(defvar *random-string-characters* "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ01234567890123456789")
(defconstant +unix-epoch-difference+ (encode-universal-time 0 0 0 1 1 1970 0))

(defun enlist (var &rest args)
  (if (listp var) var (list* var args)))

(defun delist (var &optional (n 0))
  (if (listp var) (nth n var) var))

(defun starts-with (start string)
  (and (<= (length start) (length string))
       (string= start string :end2 (length start))))

(defun ends-with (end string)
  (and (<= (length end) (length string))
       (string= end string :start2 (- (length string) (length end)))))

(declaim (inline universal-to-unix-time))
(defun universal-to-unix-time (universal-time)
  (- universal-time +unix-epoch-difference+))

(declaim (inline unix-to-universal-time))
(defun unix-to-universal-time (unix-time)
  (+ unix-time +unix-epoch-difference+))

(declaim (inline get-unix-time))
(defun get-unix-time ()
  (universal-to-unix-time (get-universal-time)))

(defun format-relative-time (stamp &optional stream)
  (when (typep stamp 'local-time:timestamp)
    (setf stamp (local-time:timestamp-to-universal stamp)))
  (if (= stamp 0)
      (format stream "0 seconds")
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
          (format stream "~{~a~^, ~}" non-NIL)))))

(defun format-clock-time (stamp &optional stream)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   stream stamp :format '(:hour ":" (:min 2) ":" (:sec 2))
                :timezone local-time:+utc-zone+))

(defun format-machine-date (stamp &optional stream)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   stream stamp :format '((:year 4) "-" (:month 2) "-" (:day 2) "T" (:hour 2) ":" (:min 2) ":" (:sec 2) "Z")
                :timezone local-time:+utc-zone+))

(defun format-human-date (stamp &optional stream)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   stream stamp :format '((:year 4) "." (:month 2) "." (:day 2) " " (:hour 2) ":" (:min 2) ":" (:sec 2))
                :timezone local-time:+utc-zone+))

(defun format-fancy-date (stamp &optional stream)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   stream stamp :format '(:long-weekday ", " :ordinal-day " of " :long-month " " :year ", " :hour ":" (:min 2) ":" (:sec 2) " UTC")
                :timezone local-time:+utc-zone+))

(defun format-only-date (stamp &optional stream)
  (when (integerp stamp) (setf stamp (local-time:universal-to-timestamp stamp)))
  (local-time:format-timestring
   stream stamp :format '((:year 4) "-" (:month 2) "-" (:day 2))
                :timezone local-time:+utc-zone+))

(defun format-time (stamp &optional (relative-time-threshold (* 60 60 24)) stream)
  (when (typep stamp 'local-time:timestamp)
    (setf stamp (local-time:timestamp-to-universal stamp)))
  (let ((now (get-universal-time)))
    (cond ((and (< (- now relative-time-threshold) stamp) (<= stamp now))
           (format stream "~a ago" (format-relative-time (- now stamp))))
          ((and (< stamp (+ now relative-time-threshold)) (< now stamp))
           (format stream "in ~a" (format-relative-time (- stamp now))))
          (T
           (format stream "at ~a" (format-human-date stamp))))))

(defun parse-time (time &key time-zone error default)
  (flet ((p (a)
           (if (and a (string/= "" a))
               (parse-integer a)
               0)))
    (or (cl-ppcre:register-groups-bind (yy mm dd h m s) ("(\\d{1,4})[-,./](\\d{1,2})[-,./](\\d{1,2})(?:[tT/ -](\\d{1,2})[:.-](\\d{1,2})(?:[:.-](\\d{1,3}))?)?" time)
          (encode-universal-time (p s) (p m) (p h) (p dd) (p mm) (p yy) time-zone))
        (cl-ppcre:register-groups-bind (h m s) ("[tT]?(\\d{1,2})[:.-](\\d{1,2})(?:[:.-](\\d{1,3}))?" time)
          (+ (p s) (* (p m) 60) (* (p h) 60 60)))
        (if error
            (error "Cannot parse ~s into a time." time)
            default))))

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

;; FIXME: Move to USER?
(defmacro perm (&rest tree)
  (let ((perm (format NIL "~{~(~a~)~^.~}" tree)))
    ;; Execute during compile
    (pushnew perm (module-permissions (module)) :test #'string=)
    ;; And ensure during load as well.
    `(load-time-value
      (progn
        (pushnew ,perm (module-permissions ,(package-name (module))) :test #'string=)
        ',tree))))

(defun copy-hash-table (table &key (test (hash-table-test table))
                                   (size (hash-table-size table))
                                   (rehash-size (hash-table-rehash-size table))
                                   (rehash-threshold (hash-table-rehash-threshold table)))
  (let ((new (make-hash-table :test test :size size :rehash-size rehash-size :rehash-threshold rehash-threshold)))
    (maphash (lambda (k v) (setf (gethash k new) v)) table)
    new))

;; FIXME: don't emit things like [], * into pathnames
;;        that the implementation might interpret badly
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
           (when (or* val)
             (write-char #\= stream)
             (url-encode val :stream stream))
           (when (cdr cons)
             (write-char #\& stream))))

(defun format-urlpart (stream arg &rest args)
  (declare (ignore args))
  (url-encode arg :stream stream :allowed "-._~!$&()*,;=:@/"))

(defun rewrite-url (url &key (schema NIL schemap)
                             (domains NIL domainsp)
                             (port NIL portp)
                             (path NIL pathp)
                             (parameters NIL parametersp)
                             (fragment NIL fragmentp))
  (let ((url (etypecase url
               (string (puri:parse-uri url))
               (puri:uri url)
               (uri (puri:parse-uri (uri-to-url url :schema schema))))))
    (when schemap (setf (puri:uri-scheme url) schema))
    (when domainsp (setf (puri:uri-host url) (format NIL "~{~a~^.~}" (reverse domains))))
    (when portp (setf (puri:uri-port url) port))
    (when pathp (setf (puri:uri-path url) (format NIL "~/radiance-core::format-urlpart/" path)))
    (when parametersp (setf (puri:uri-query url) (format NIL "~/radiance-core::format-query/" parameters)))
    (when fragmentp (setf (puri:uri-fragment url) (format NIL "~/radiance-core::format-urlpart/" fragment)))
    (puri:render-uri url NIL)))

(defun merge-url (url &key schema domains port path parameters fragment)
  (let ((url (etypecase url
               (string (puri:parse-uri url))
               (puri:uri url)
               (uri (puri:parse-uri (uri-to-url url :schema schema))))))
    (when schema (setf (puri:uri-scheme url) schema))
    (when domains (setf (puri:uri-host url) (format NIL "~{~a~^.~}~@[.~a~]"
                                                    (reverse domains) (puri:uri-host url))))
    (when port (setf (puri:uri-port url) port))
    (when path (setf (puri:uri-path url) (format NIL "~@[~a/~]~/radiance-core::format-urlpart/"
                                                 (puri:uri-path url) path)))
    (when parameters (setf (puri:uri-query url) (format NIL "~/radiance-core::format-query/~@[&~a~]"
                                                        parameters (puri:uri-query url))))
    (when fragment (setf (puri:uri-fragment url) (format NIL "~/radiance-core::format-urlpart/" fragment)))
    (puri:render-uri url NIL)))

(defun ecompile (name &optional definition)
  (multiple-value-bind (function warnings-p failure-p) (compile name definition)
    (if failure-p
        (error "Compilation of ~a with definition~%  ~s~%failed." name definition)
        (values function warnings-p failure-p))))

(defmacro with-actions ((error info) action-clauses &body body)
  (let ((action (gensym "ACTION")))
    `(let ((,error) (,info)
           (,action (post/get "action")))
       (declare (ignorable ,error ,info))
       (handler-case
           (cond
             ,@(loop for (clause . body) in action-clauses
                     collect `((string-equal ,action ,(string clause)) ,@body)))
         (,error (err)
           (setf ,error err)))
       ,@body)))

(indent:define-indentation with-actions (6 (&whole 4 &rest) &body))

(defmacro call (pkg sym &rest args)
  `(funcall (find-symbol ,(string sym) ,(string pkg)) ,@args))

(defun check-for-shared-symbol (symbol)
  (when (not (eql *package* (symbol-package symbol)))
    (warn 'definition-for-shared-package :symbol symbol)))
