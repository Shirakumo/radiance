#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(define-condition mechanism-method-missing (error)
  ((%method-name :initarg :method-name :accessor method-name))
  (:report (lambda (c s) (format s "Mechanism does not implement the method ~s." (method-name c)))))

(defclass mechanism ()
  ((name :initarg :name :initform "UNNAMED" :accessor name :allocation :class)))

(defun get-mechanisms ()
  (mapcar #'(lambda (a) (make-instance (class-name a))) (closer-mop:class-direct-subclasses (find-class 'mechanism))))

(defgeneric page-login (mechanism)
  (:documentation "Called when the login page is built."))

(defgeneric page-register (mechanism)
  (:documentation "Called when the register page is built."))

(defgeneric page-settings (mechanism)
  (:documentation "Called when the admin settings panel is built."))

(defgeneric linked-p (mechanism user)
  (:documentation "Called to check during the registration process whether the mechanism has been linked or not. Should return a generalized boolean."))

(defgeneric finalize-registration (mechanism user)
  (:documentation "Called to finalize the registration process and save data about a user."))

(defmethod page-login ((mechanism mechanism))
  (warn 'mechanism-method-missing :method-name 'page-login))

(defmethod page-register ((mechanism mechanism))
  (warn 'mechanism-method-missing :method-name 'page-register))

(defmethod page-settings ((mechanism mechanism))
  (warn 'mechanism-method-missing :method-name 'page-login))

(defmethod linked-p ((mechanism mechanism) user)
  (warn 'mechanism-method-missing :method-name 'page-login))

(defmethod finalize-registration ((mechanism mechanism) user)
  (warn 'mechanism-method-missing :method-name 'finalize-registration))

(define-interface-extension auth
  (error-out (process code text)
    (:documentation "Error out of a login/registration process and redirect to the proper page. Process should be one of (:LOGIN :REGISTER)."))
  (with-redirecting ((process) &body body)
    (:type :macro)
    (:documentation "Automatically catches any error of type RADIANCE-ERROR and executes the proper ERROR-OUT method."))
  (define-mechanism (name &body forms)
    (:type :macro)
    (:documentation "Define a mechanism. Expected as body forms are (:LOGIN form*) (:REGISTER form*) (:SETTINGS form*) (:LINKED-P form*) (:FINALIZE form*).")))

(radiance::define-indentation 'define-mechanism '(4 &rest (&whole 2 &lambda 4 &body)))
(radiance::initialize-slime)

(define-interface-method auth:error-out ((process (eql :login)) code text)
  (server:redirect (format NIL "/login?errortext=~a&errorcode=~a" text code)))

(define-interface-method auth:error-out ((process (eql :register)) code text)
  (server:redirect (format NIL "/register?errortext=~a&errorcode=~a" text code)))

(define-interface-method auth:with-redirecting (options &body body)
  (destructuring-bind (process) options
    (assert (find process '(:login :register)) () "PROCESS has to be one of (:LOGIN :REGISTER).")
    `(handler-case
         (progn ,@body)
       (radiance-error (err)
         (auth:error-out ,process (code err) (text err))))))

(define-interface-method auth:define-mechanism (name &body forms)
  (let ((login (cdr (assoc :login forms)))
        (register (cdr (assoc :register forms)))
        (settings (cdr (assoc :settings forms)))
        (linked-p (cdr (assoc :linked-p forms)))
        (finalize (cdr (assoc :finalize forms))))
    `(progn
       (defclass ,name (mechanism) ((name :initform ,(string name) :accessor name)))
       ,(when login
          `(defmethod page-login ((,(caar login) ,name) ,@(cdar login))
             ,@(cdr login)))
       ,(when register
          `(defmethod page-register ((,(caar register) ,name) ,@(cdar register))
             ,@(cdr register)))
       ,(when settings
          `(defmethod page-settings ((,(caar settings) ,name) ,@(cdar settings))
             ,@(cdr settings)))
       ,(when linked-p
          `(defmethod linked-p ((,(caar linked-p) ,name) ,@(cdar linked-p))
             ,@(cdr linked-p)))
       ,(when finalize
          `(defmethod finalize-registration ((,(caar finalize) ,name) ,@(cdar finalize))
             ,@(cdr finalize))))))
