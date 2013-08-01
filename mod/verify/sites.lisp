#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defun get-redirect (&optional (default "/") (request *radiance-request*))
  (or (hunchentoot:get-parameter "redirect" request)
      (hunchentoot:post-parameter "redirect" request)
      (if *radiance-session* (session-field *radiance-session* "redirect"))
      (hunchentoot:referer request)
      default))

(define-condition auth-login-error (auth-error) ())
(define-condition auth-register-error (auth-error) ())

(defpage login #u"auth./login" (:lquery (template "verify/login.html"))
  (ignore-errors (authenticate T))
  (if (or (not *radiance-session*) (session-temp-p *radiance-session*))
      (loop with target = ($ "#content")
         for mechanism being the hash-values of *verify-mechanisms*
         do ($ target (append (show-login mechanism))))
      ($ "#mechanisms" (append (parse-html "<li id=\"loggedin\"><h2>You are already logged in.</h2></li>")))))
        
(defpage logout #u"auth./logout" ()
  (ignore-errors (authenticate T))
  (if *radiance-session*
      (session-end *radiance-session*))
  (hunchentoot:redirect (get-redirect)))

(defpage register #u"auth./register" (:lquery (template "verify/register.html"))
  (ignore-errors (authenticate T))
  (loop with target = ($ "#content")
     for mechanism being the hash-values of *verify-mechanisms*
     do ($ target (append (show-register mechanism)))))

(defpage register/finish #u"auth./register/finish"  ()
  (ignore-errors (authenticate T))
  (if (hunchentoot:post-parameters *radiance-request*)
      (session-field *radiance-session* "post-data" :value (hunchentoot:post-parameters *radiance-request*)))
  (handler-case
      (if (string= (hunchentoot:post-parameter "action" *radiance-request*) "Register")
          (let ((username (hunchentoot:post-parameter "username" *radiance-request*))
                (displayname (hunchentoot:post-parameter "displayname" *radiance-request*)))
            (if (and username (> (length username) 0))
                (let ((user (user-get (implementation 'user) username)))
                  (if (not (user-saved-p user))
                      (progn
                        (if (or (not displayname) (= (length displayname) 0))
                            (setf displayname username))
                        (user-field user "displayname" :value displayname)
                        (user-field user "register-date" :value (get-unix-time))
                        (user-field user "secret" :value (make-random-string))
                        (loop for mechanism being the hash-values of *verify-mechanisms*
                           do (handle-register mechanism user))
                        (user-save user))
                      (error 'auth-register-error :text "Username already taken!" :code 17)))
                (error 'auth-register-error :text "Username required!" :code 16)))
          (error 'auth-register-error :text "Nothing to do!" :code 15))
    (auth-error (err)
      (hunchentoot:redirect (format nil "/register?errorcode=~a&errortext=~a" (slot-value err 'radiance::code) (slot-value err 'radiance::text)))))
  (hunchentoot:redirect "/register"))
