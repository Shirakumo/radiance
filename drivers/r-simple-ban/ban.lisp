#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-ban
  (:use #:cl #:radiance)
  (:implements #:ban))
(in-package #:simple-ban)

(defvar *ban-file* (data-file "bans.txt"))
(defvar *bans* (make-hash-table :test 'equalp))

(defun load-bans ()
  (with-open-file (stream *ban-file* :direction :input :if-does-not-exist NIL)
    (when stream
      (loop for line = (read-line stream NIL NIL)
            while line
            do (when (string/= line "")
                 (let* ((space (position #\Space line))
                        (ip (subseq line 0 space))
                        (time (subseq line (1+ space))))
                   (setf (gethash ip *bans*)
                         (if (string= ip "T")
                             T
                             (parse-integer time))))))))
  *bans*)

(defun save-bans ()
  (with-open-file (stream *ban-file* :direction :output :if-exists :supersede)
    (loop for ip being the hash-keys of *bans*
          for time being the hash-values of *bans*
          do (format stream "~a ~a~%" ip time)))
  *bans*)

(define-trigger radiance:server-start ()
  (load-bans))

(defun ban:jail (ip &key (duration T))
  (v:info :ban "Jailing ~a for ~a" ip duration)
  (setf (gethash ip *bans*)
        (if (integerp duration)
            (+ (get-universal-time) duration)
            T))
  (save-bans)
  ip)

(defun ban:list ()
  (loop for ip being the hash-keys of *bans*
        for time being the hash-values of *bans*
        collect (list ip time)))

(defun ban:jail-time (&optional (ip (remote *request*)))
  (gethash ip *bans*))

(defun ban:release (ip)
  (v:info :ban "Releasing ~a" ip)
  (remhash ip *bans*)
  (save-bans)
  ip)

(define-trigger radiance:request (request response)
  (declare (ignore response))
  (let ((limit (gethash (remote request) *bans*)))
    (when limit
      (if (or (not (integerp limit)) (< (get-universal-time) limit))
          (error 'request-denied :message (format NIL "~:[You have been banned.~;You have been banned until ~:*~a.~]"
                                                  (when (integerp limit) (format-universal-time limit))))
          (ban:release (remote request))))))

(define-implement-hook admin
  (admin:define-panel bans admin (:access (radiance admin bans) :icon "fa-ban" :tooltip "Manage banned IP addresses." :lquery (template "bans.ctml"))
    (with-actions (error info)
        ((:release
          (dolist (ip (or (post/get "selected[]") (list (post/get "ip"))))
            (ban:release ip))
          (setf info "IPs released."))
         (:jail
          (ban:jail
           (post/get "ip")
           :duration (if (or (not (post/get "jail-time"))
                             (string-equal (post/get "jail-time") "")
                             (string-equal (post/get "jail-time") "T"))
                         T
                         (parse-integer (post/get "jail-time"))))
          (setf info "IP jailed.")))
      (r-clip:process
       T
       :bans (ban:list)
       :info info))))
