#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmacro verify-multiple (&rest checks)
  ""
  `(loop for (function variable &rest arg) in ,checks
      unless (apply (function function) variable arg)
      collect variable))

;; very crude... should see about optimizing it.
(defun username-p (string)
  (and string
       (user:get string)))

(defun displayname-p (string)
  (and string
       (let ((length (length string)))
         (and (> length 0)
              (<= length 32)))))

(defun domain-p (string)
  (and string
       (let ((len (length string)))
         (> len 4)
         (let ((pos (search "." string :from-end T)))
           (and pos
                (> pos 1)
                (< pos (- len 2)))))))
              

(defun url-p (string &key (fqdn-p T))
  (and string
       (> (length string) 7)
       (string= (subseq string 0 7) "http://")
       (or (not fqdn-p)
           (and (find #\. string)
                (> (length string) 12)))
       T))

(defun email-p (string &key (fqdn-p T))
  (and string
       (let ((parts (split-sequence:split-sequence #\@ string)))
         (and (not (cddr parts))
              (> (length (first parts)) 0)
              (> (length (second parts)) 0)
              (or (not fqdn-p)
                  (and (find #\. (second parts))
                       (> (length (second parts)) 6)))))))

(defun date-p (string &key (delimiter #\-) (format "YMD"))
  (and string
       (let ((parts (split-sequence:split-sequence delimiter string)))
         (and (= (length parts) (length format))
              (loop for i below (length format)
                 always (%date-p (nth i parts) (char format i)))))))

(defun %date-p (string type)
  (let ((num (parse-integer string :junk-allowed T)))
    (and num
         (case type
           (#\Y (>= (length string) 4))
           (#\M (and (<= num 12) (= (length string) 2)))
           (#\m (and (<= num 12) (<= (length string) 2)))
           (#\D (and (<= num 31) (= (length string) 2)))
           (#\d (and (<= num 31) (<= (length string) 2)))
           (otherwise T)))))

(defun date-to-timestamp (date)
  "Turns a string date into a unix timestamp."
  (local-time:timestamp-to-unix (local-time:parse-timestring date)))

(defun timestamp-to-date (timestamp &optional (format '((:year 4) #\- (:month 2) #\- (:day 2))))
  "Turns a unix timestamp into a datestring. Default format is as required by the input type date in HTML5."
  (local-time:format-timestring NIL (local-time:unix-to-timestamp timestamp) :format format))

(defun timestamp-to-datetime (timestamp)
  "Turns a unix timestamp into a human readable datestring."
  (timestamp-to-date timestamp '(:year #\. :month #\. :day #\space :hour #\: :min #\: :sec)))
