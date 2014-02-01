#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defvar *verify-mechanisms* (make-hash-table))

(defclass mechanism ()
  () (:documentation "Class to represent authentication mechanisms."))

(defgeneric show-login (mechanism)
  (:documentation "Inserts all required login HTML data into the target node."))

(defgeneric show-register (mechanism)
  (:documentation "Inserts all required register HTML data into the target node."))

(defgeneric handle-register (mechanism user)
  (:documentation "Handles the registration process."))

(defgeneric show-options (mechanism target)
  (:documentation "Show and handle the options page."))

(defmacro defmechanism (name &optional description &body bodies)
  "Defines a new authentication mechanism. Required bodies: show-login show-register handle-login handle-register"
  (if (not (stringp description)) (setf description NIL bodies (cons description bodies)))
  (let ((classname (make-symbol (format NIL "MECHANISM-~a" (string-upcase name)))))
    `(progn
       (defclass ,classname (mechanism)
         () (:documentation ,description))
       ,@(loop for body in bodies collect
              `(defmethod ,(first body) ((mechanism ,classname) ,@(second body))
                 ,@(cddr body)))
       (setf (gethash ,(make-keyword name) *verify-mechanisms*) (make-instance ',classname)))))

(defgeneric get-mechanism (name)
  (:documentation "Retrieves a mechanism."))

(defmethod get-mechanism ((name symbol))
  (gethash name *verify-mechanisms*))

(defmethod get-mechanism ((name string))
  (get-mechanism (make-keyword name)))

(define-condition auth-session-error (auth-error) ())

(define-interface-method auth:authenticate ()
  (let ((token (server:cookie "token")))
    (if (and token (> (length token) 0) (not *radiance-session*))
        (progn 
          ;; Decrypt token with global key to get user and session data
          (setf token (rad-crypto:decrypt (parse-integer token :radix 36) (config-tree :verify :session :secret))) 
          (if (and token (find #\- token))
              (let* ((username (subseq token 0 (position #\- token)))
                     (user (user:get username))
                     (token (subseq token (1+ (position #\- token)))))
                (if (user:saved-p user)
                    (authenticate-user user token)
                    (error 'auth-session-error :text (format nil "Unknown user: ~a" username) :code 2)))
              (error 'auth-session-error :text (format nil "Malformed token: ~a" token) :code 1))))))

(defun authenticate-user (user token)
  ;; If per user secrets are activated, decrypt session data with the secret.
  (setf token (if (config-tree :verify :session :use-per-user-secret)
                  (rad-crypto:decrypt token (user:field user "secret"))
                  token))
  (let ((data (split-sequence:split-sequence #\: token)))
    (if (= (length data) 3)
        (destructuring-bind (timestamp random session-id) data
          (declare (ignore random))
          (let ((session (session:get session-id)))
            (if session
                (if (= (parse-integer timestamp) (session-time session))
                    (progn (v:debug :verify.auth "User ~a successfully authenticated session ~a (initiated on ~a)" user session-id timestamp)
                           (setf (s-user session) user)
                           (setf *radiance-session* session)
                           session)
                    (error 'auth-session-error :text (format nil "Timestamp mismatch: ~a ~a" timestamp (session-time session)) :code 6))
                (progn
                  (server:set-cookie "token")
                  (error 'auth-session-error :text (format nil "Invalid session: ~a" session-id) :code 5)))))
        (error 'auth-session-error :text (format nil "Invalid data length: ~a" (length data)) :code 4))))


(defun auth-page (page redirect)
  (format nil "http://auth.~a:~a/~a?redirect=~a" (domain *radiance-request*) (port *radiance-request*) page redirect))

(define-interface-method auth:page-login (&key redirect)
  (auth-page "login" redirect))

(define-interface-method auth:page-logout (&key redirect)
  (auth-page "logout" redirect))

(define-interface-method auth:page-register (&key redirect)
  (auth-page "register" redirect))

#|
(define-interface-method auth:page-options (&key target)
  )
|#
