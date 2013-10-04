#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defclass request-continuation ()
  ((id :initarg :id :initform (uuid:make-v4-uuid) :reader id)
   (name :initarg :name :initform "CONT" :accessor name)
   (request :initarg :request :initform (error "Request required.") :reader request)
   (function :initarg :function :initform (error "Function required.") :reader continuation-function))
  (:documentation "Request Continuation object."))

(defmethod print-object ((cont request-continuation) out)
  (print-unreadable-object (cont out :type T)
    (format out "~a ~a ~a" (name cont) (id cont) (request cont))))

(defun get-continuation (id &optional (session *radiance-session*))
  (if (and session (session-field session 'CONTINUATIONS))
      (gethash id (session-field session 'CONTINUATIONS))))

(defun make-continuation (function &key (id (uuid:make-v4-uuid)) (name "CONT") (request *radiance-request*) (session *radiance-session*))
  (if (null (session-field session 'CONTINUATIONS))
      (setf (session-field session 'CONTINUATIONS) (make-hash-table)))
  (setf (gethash id (session-field session 'CONTINUATIONS))
        (make-instance 'request-continuation
                       :name name :id id
                       :request request
                       :function function)))

;; Macro to build request continuations
(defmacro with-request-continuation ((&key (request *radiance-request*) (name "CONT") (session *radiance-session*)) &body continuation)
  "Builds a request-continuation and returns the generated continuation ID."
  (let ((funcsym (gensym "FUNCTION"))
        (contsym (gensym "CONTINUATION"))
        (cidsym (gensym "cID")))
    `(let ((cont (make-continuation (lambda () ,@continuation) )))))

;; Sample usage case
(let ((id (with-request-continuation ()
            (if (string= (get-var "sure" *radiance-cont-request*) "Yes")
                (yes-case)
                (no-case)))))
  (make-form-with-id-bla id))

;; Excerpt from the modified handler function
(let* ((cid (get-or-post-var "cID"))
       (conts (session-field *radiance-session* 'CONTINUATIONS))
       (cont (gethash cid conts)))
  (if cont
      (with-slots (request function) cont
        (let ((*radiance-cont-request* *radiance-request*)
              (*radiance-request* request))
          (funcall function)
          (remhash cid conts)))
      (execute-normal-operations)))


#|
  Request

  Radiance Handler

  Dispatcher

  Module-Fun

  (confirm) Macro-call <ID>

  Page render

  -> Submit

  Request

  Radiance Handler
 
  Goto <ID>


  Pseudo continuations through request objects and continuation IDs?
  Implement through sessions to ensure security... Hrm.

  Q: Multi-page requests?
  Should be no problem, a new token gets generated each page call
  request continuations get retrieved by session and token, so that
  should be secure.

  Q: Browser navigation?
  Browser navigation back to request ID issuing pages poses no problem.
  Browser navigation ahead to request ID landing pages is a problem due
  to validity.

  Q: Validity?
  Request continuation could expire once it's requested for the first time
  although that poses problems with F5-ing and resubmitting the data, as
  the token would be invalidated.
|#
