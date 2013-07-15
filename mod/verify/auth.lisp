#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defvar *verify-mechanisms* (make-hash-table))

(defclass mechanism ()
  () (:documentation "Class to represent authentication mechanisms."))

(defgeneric show-login (mechanism target)
  (:documentation "Inserts all required login HTML data into the target node."))

(defgeneric show-register (mechanism target)
  (:documentation "Inserts all required register HTML data into the target node."))

(defgeneric handle-login (mechanism redirect)
  (:documentation "Handles the login process and redirects to the requested page."))

(defgeneric handle-register (mechanism redirect)
  (:documentation "Handles the register process and redirects to the requested page."))

(defmacro defmechanism (name &optional description &rest bodies)
  "Defines a new authentication mechanism. Required bodies: show-login show-register handle-login handle-register"
  (if (not (stringp description)) (setf description NIL bodies (cons description bodies)))
  (let ((classname (make-symbol (format NIL "mechanism-~a" (string-downcase name)))))
    `(progn
       (defclass ,classname (mechanism)
         () (:documentation description))
       ,@(loop for body in bodies collect
              `(defmethod ,(first body) ((mechanism ,classname) ,@(second body))
                 ,@(cddr body)))
       (setf (gethash ,(make-keyword name)) (make-instance ',classname)))))

(defgeneric get-mechanism (name)
  (:documentation "Retrieves a mechanism."))

(defmethod get-mechanism ((name symbol))
  (gethash name *verify-mechanisms*))

(defmethod get-mechanism ((name string))
  (get-mechanism (make-keyword name)))
