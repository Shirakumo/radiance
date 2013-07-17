#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defgeneric page-login (verify))
(defgeneric page-auth (verify))
(defgeneric page-logout (verify))
(defgeneric page-register (verify))

(defun get-redirect (&optional (request *radiance-request*) (default "/"))
  (or (hunchentoot:get-parameter "redirect" request)
      (hunchentoot:post-parameter "redirect" request)
      (hunchentoot:referer request)
      default))

(define-condition auth-login-error (auth-error) ())

(defmethod page-login ((verify verify))
  ($ (initialize (template "verify/login.html")))
  (loop with target = ($ "#mechanisms")
     for mechanism being the hash-values of *verify-mechanisms*
     do ($ target (append (show-login mechanism))))
  (first ($ (serialize :doctype "html"))))

(defmethod page-auth ((verify verify))
  (let* ((name (third (split-sequence:split-sequence #\/ (path *radiance-request*))))
         (mechanism (get-mechanism (make-keyword name))))
    (if mechanism 
        (handler-case (if (handle-login mechanism)
                          (hunchentoot:redirect (get-redirect))
                          (error 'auth-login-error :text "Login failed." :code 9))
          (auth-error (err)
            (hunchentoot:redirect (format nil "/login?mechanism=~a&errorcode=~a&errortext=~a" name (slot-value err 'radiance::code) (slot-value err 'radiance::text)))))
        (error 'auth-error :text (format nil "Unknown authentication mechanism ~a" name) :code 8))))
        
(defmethod page-logout ((verify verify))
  (if *radiance-session*
      (session-end *radiance-session*))
  (hunchentoot:redirect (get-redirect)))

(defhook 'verify-login (get-module :verify) #'page-login)
(defhook 'verify-auth (get-module :verify) #'page-auth)
(defhook 'verify-logout (get-module :verify) #'page-logout)
(defhook 'verify-register (get-module :verify) #'page-register)

(register (implementation 'dispatcher) 'verify-login :subdomain "auth" :path "/login")
(register (implementation 'dispatcher) 'verify-auth :subdomain "auth" :path "/auth")
(register (implementation 'dispatcher) 'verify-logout :subdomain "auth" :path "/logout")
(register (implementation 'dispatcher) 'verify-register :subdomain "auth" :path "/register")
