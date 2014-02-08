#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(define-condition auth-register-error (auth-error) ())

(core:define-page main-login #u"auth./login" (:lquery (template "verify/login.html"))
  (ignore-errors (auth:authenticate))
  (when (server:get "errortext")
    ($ "#error" (html (concatenate 'string "<i class=\"icon-remove-sign\"></i> " (server:get "errortext")))))
  (when (and *radiance-session* (not (auth:authenticated-p)))
    (session:end *radiance-session*)
    (setf *radiance-session* NIL))
  (if (or (not *radiance-session*) (session:temp-p *radiance-session*))
      (loop with target = ($ "#content")
            with panel = ($ "#panel ul")
            for mechanism in (get-mechanisms)
            do ($ target (append (page-login mechanism)))
               ($ panel (append (lquery:parse-html (format NIL "<li class=\"~a\"><a>~:*~a</a></li>" (name mechanism))))))
      (server:redirect (make-uri (or (config-tree :verify :register :endpoint)
                                     "/")))))

(core:define-page main-logout #u"auth./logout" ()
  (ignore-errors (auth:authenticate))
  (when *radiance-session*
    (user:action "Logout")
    (session:end *radiance-session*))
  (server:redirect (server:referer)))

(core:define-page main-register #u"auth./register" (:lquery (template "verify/register.html"))
  (ignore-errors (auth:authenticate))
  (if (or (not *radiance-session*) (session:temp-p *radiance-session*))
      (progn
        (when (server:get "errortext")
          ($ "#error" (html (concatenate 'string "<i class=\"icon-remove-sign\"></i> " (server:get "errortext")))))
        (loop with target = ($ "#content")
              with panel = ($ "#panel .logins ul")
              for mechanism in (get-mechanisms)
              do ($ target (append (page-register mechanism)))
                 ($ panel (append (lquery:parse-html (format NIL "<li class=\"~a\"><a>~:*~a</a></li>" (name mechanism))))))
        (trigger :user :register-page))
      (server:redirect (make-uri (or (config-tree :verify :register :endpoint)
                                     "/")))))

(core:define-page register/finish #u"auth./register/finish"  ()
  (ignore-errors (auth:authenticate))
  (unless *radiance-session*
    (setf *radiance-session* (session:start-temp)))
  (when (server:posts)
    (session:field *radiance-session* "post-data" :value (server:posts)))
  (handler-case
      (server:with-posts (action username displayname email
                          hidden firstname address)
        (unless (string= action "Register")
          (error 'auth-register-error :text "Nothing to do!" :code 15))
        (unless (and username (> (length username) 0)
                     email (> (length email) 0) (email-p email))
          (error 'auth-register-error :text "Username and email required!" :code 16))
        (let ((user (user:get username)))
          (when (user:saved-p :user user)
            (error 'auth-register-error :text "Username already taken!" :code 17))
          (unless (and (string= hidden "hidden")
                       (string= address "fixed")
                       (string= firstname ""))
            (error 'auth-register-error :text "Hidden fields modified!" :code 18))
          (when (or (not displayname) (= (length displayname) 0))
            (setf displayname username))
          (setf (user:field user "displayname") displayname
                (user:field user "register-date") (get-unix-time)
                (user:field user "email") email
                (user:field user "secret") (make-random-string)
                (user:field user "perms") (concatenate-strings (config-tree :verify :register :defaultperms) #\Newline))
          (unless (loop for mechanism in (get-mechanisms)
                        if (progn
                             (finalize-registration mechanism user)
                             (linked-p mechanism user))
                          do (return T))
            (error 'auth-register-error :text "At least one login required!" :code 19))
          (v:debug :verify.user "Creating new user ~a" username)
          (dm:insert (model user))
          (user:action "Register" :public T :user user)
          (session:end *radiance-session*)
          (session:start username)
          (server:redirect (make-uri (or (config-tree :verify :register :endpoint)
                                         "/")))))
    (radiance-error (err)
      (server:redirect (format nil "/register?errorcode=~a&errortext=~a&panel=profile" (slot-value err 'radiance::code) (slot-value err 'radiance::text))))))
