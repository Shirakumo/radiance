#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-rate
  (:use #:cl #:radiance)
  (:implements #:rate))
(in-package #:simple-rate)

(defvar *rates* (make-hash-table))

(define-trigger db:connected ()
  ;; See http://stackoverflow.com/a/7477384/743237 for the IP length
  (db:create 'simple-rates '((rate (:varchar 64))
                             (time (:integer 5))
                             (limit :integer)
                             (ip (:varchar 45)))
             :indices '(rate (rate ip))))

(defclass rate ()
  ((name :initarg :name :initform (error "NAME required.") :accessor name)
   (timeout :initarg :timeout :initform 60 :accessor timeout)
   (limit :initarg :limit :initform 1 :accessor limit)
   (exceeded :initarg :exceeded :initform #'(lambda (limit) (error "Please wait ~d seconds." limit)) :accessor exceeded)))

(defun db-rate-name (name)
  (let ((name (format NIL "~a:~a"
                      (package-name (symbol-package name))
                      (symbol-name name))))
    (if (<= (length name) 64)
        name
        (error "Rate name too long."))))

(defun rate (name)
  (gethash name *rates*))

(defun (setf rate) (rate name)
  (setf (gethash name *rates*)
        rate))

(defmacro rate:define-rate (name (time-left &key (timeout 60) (limit 1)) &body on-limit-exceeded)
  `(eval-when (:compile-toplevel :load-toplevel :execute)
     (setf (rate ',name)
           (make-instance
            'rate
            :name ,(db-rate-name name)
            :limit ,limit
            :timeout ,timeout
            :exceeded #'(lambda (,time-left) ,@on-limit-exceeded)))))

(defun rate:rate-left (rate &key (ip (remote *request*)))
  (let* ((rate (rate rate))
         (limit (dm:get-one 'simple-rates (db:query (:and (:= 'name (name rate))
                                                          (:= 'ip ip))))))
    (values (dm:field limit "limit")
            (- (+ (dm:field limit "time")
                  (timeout rate))
               (get-universal-time)))))

(defun rate::tax-rate (rate &key (ip (remote *request*)))
  (let* ((rate (rate rate))
         (limit (dm:get-one 'simple-rates (db:query (:and (:= 'name (name rate))
                                                          (:= 'ip ip))))))
    ;; If we're out of attempts, but the time has also passed, reset.
    (when (and (<= 0 (dm:field limit "limit"))
               (<= (+ (dm:field limit "time") (timeout rate)) (get-universal-time)))
      (setf (dm:field limit "limit") (limit rate)))
    ;; Tax it.
    (decf (dm:field limit "limit"))
    (setf (dm:field limit "time") (get-universal-time))
    (dm:save limit)))

(defmacro rate:with-rate-limitation ((rate) &body body)
  (let ((amount (gensym "AMOUNT"))
        (timeout (gensym "TIMEOUT")))
    `(multiple-value-bind (,amount ,timeout) (rate:rate-left ',rate)
       (if (and (<= 0 ,amount) (< 0 ,timeout))
           (funcall ,(exceeded (rate rate)) ,timeout)
           (progn
             (rate::tax-rate ',rate)
             ,@body)))))
