#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-sessions
  (:use #:cl #:radiance)
  (:implements #:session))
(in-package #:simple-sessions)

(defvar *session-table* (make-hash-table :test 'equalp))
(defvar *session-key* (make-random-string))
(defvar *session-timeout-format* '((:year 4) #\. (:month 2) #\. (:day 2) #\Space (:hour 2) #\: (:min 2) #\: (:sec 2)))

(defclass session (session:session)
  ((id :initarg :id :initform (princ-to-string (uuid:make-v4-uuid)) :accessor id)
   (fields :initarg :fields :initform (make-hash-table :test 'eql) :accessor fields)
   (timeout :initarg :timeout :initform (+ (get-universal-time) session:*default-timeout*) :accessor timeout)))

(defmethod print-object ((session session) stream)
  (print-unreadable-object (session stream :type T)
    (format stream "~a " (id session))
    (local-time:format-timestring stream (local-time:universal-to-timestamp (timeout session)) :format *session-timeout-format*)))

(defun make-cookie-value (session)
  (cryptos:encrypt (format NIL "~a-~a" (id session) (make-random-string (+ 4 (random 9)))) *session-key*))

(defmethod initialize-instance :after ((session session) &key)
  (l:debug :session "Starting session ~a" session)
  (setf (gethash (id session) *session-table*) session)
  (trigger 'session:create session)
  (when (and (boundp '*request*) (boundp '*response*))
    (setf (cookie "radiance-session" :domain (domain *request*) :path "/" :timeout (timeout session) :http-only T)
          ;; Note: Add support for the secure flag through https options in the main framework
          (make-cookie-value session))))

(defun decode-session (hash)
  (ignore-errors
   (let ((hash (cryptos:decrypt hash *session-key*)))
     (when (< 36 (length hash))
       (let ((session (session:get (subseq hash 0 36))))
         (when session
           (l:debug :session "Resuming session ~a" session)
           session))))))

(defun session:start ()
  (let ((cookie (cookie "radiance-session")))
    (or (and cookie (decode-session cookie))
        (make-instance 'session))))

(defun session:list ()
  (loop for session being the hash-values of *session-table*
        collect session))

(defun session:get (session-id)
  (let ((session (gethash session-id *session-table*)))
    (when session
      (or (and (session:active-p session) session)
          (not (session:end session))))))

(defun session:id (session)
  (id session))

(defun session:field (session field)
  (gethash field (fields session)))

(defun (setf session:field) (value session field)
  (setf (gethash field (fields session)) value))

(defun session:timeout (session)
  (timeout session))

(defun (setf session:timeout) (seconds session)
  (setf (timeout session) seconds))

(defun session:end (session)
  (setf (timeout session) 0)
  (remhash (id session) *session-table*)
  session)

(defun session:active-p (session)
  (and (< (get-universal-time) (timeout session))
       session))

(define-trigger request ()
  (setf *session* (session:start)))
