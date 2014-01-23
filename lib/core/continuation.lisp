#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass request-continuation ()
  ((id :initarg :id :initform (format NIL "~a" (uuid:make-v4-uuid)) :reader id)
   (name :initarg :name :initform "CONT" :accessor name)
   (timeout :initarg :time :initform (+ (get-unix-time) *radiance-continuation-lifetime*) :accessor timeout)
   (request :initarg :request :initform (error "Request required.") :reader request)
   (function :initarg :function :initform (error "Function required.") :reader continuation-function))
  (:documentation "Request Continuation object."))

(defmethod print-object ((cont request-continuation) out)
  (print-unreadable-object (cont out :type T)
    (format out "~a ~a ~a" (name cont) (id cont) (request cont)))
  cont)

(defun continuation (id &optional (session *radiance-session*))
  (if (and session (session:field session 'CONTINUATIONS))
      (gethash id (session:field session 'CONTINUATIONS))))

(defun continuations (&optional (session *radiance-session*))
  (if (and session (session:field session 'CONTINUATIONS))
      (alexandria:hash-table-values (session:field session 'CONTINUATIONS))))

(defun make-continuation (function &key (id (format NIL "~a" (uuid:make-v4-uuid))) (name "CONT") (request *radiance-request*) (session *radiance-session*))
  (if (null (session:field session 'CONTINUATIONS))
      (session:field session 'CONTINUATIONS :value (make-hash-table :test 'equal)))
  (v:debug :radiance.server.continuations "Creating continuation ~a ~a " name id)
  (setf (gethash id (session:field session 'CONTINUATIONS))
        (make-instance 'request-continuation
                       :name name :id id
                       :request request
                       :function function)))

(defun clean-continuations (&optional (session *radiance-session*))
  (when (and session (session:field session 'CONTINUATIONS))
    (let ((conts (session:field session 'CONTINUATIONS)))
      (loop for key being the hash-keys of conts
         for val being the hash-values of conts
         if (> (get-unix-time) (timeout val))
         do (remhash key conts)
           (v:debug :radiance.server.continuations "Removing continuation ~a due to timeout (~a > ~a)" val (get-unix-time) (timeout val))))))

(defun clean-continuations-globally ()
  (dolist (session (session:get-all))
    (clean-continuations session)))

;; Macro to build request continuations
(defmacro with-request-continuation ((&key (request '*radiance-request*) (session '*radiance-session*) (new-request-var (gensym "NEW-REQUEST")) (name "CONT")) &body continuation)
  "Builds a request-continuation and returns the generated continuation ID."
  (let ((funcsym (gensym "FUNCTION"))
        (contsym (gensym "CONTINUATION"))
        (reqsym (gensym "OLD-REQUEST")))
    `(let* ((,reqsym ,request)
            (,funcsym (lambda () 
                        (let ((,new-request-var *radiance-request*)
                              (*radiance-request* ,reqsym))
                          (declare (ignorable ,new-request-var))
                          ,@continuation)))
            (,contsym (make-continuation ,funcsym :name ,name :request ,request :session ,session)))
       (id ,contsym))))
