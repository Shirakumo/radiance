#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-password)

(define-hook (:server :init) (:documentation "Initialize verify-password storage.")
  (db:create "linked-passwords" '(("salt" :varchar 32) ("hash-type" :varchar 16) ("hash-date" :integer) ("hash" :varchar 128) ("username" :varchar 32))))

(defun make-password-hash (password &optional (salt (config-tree :verify :password :salt)) (algorithm (config-tree :verify :password :algorithm)))
  ""
  (if (string-equal algorithm "pbkdf2")
      (rad-crypto:pbkdf2-hash password salt)
      (if (find-symbol algorithm :ironclad)
          (rad-crypto:simple-hash password salt :digest (find-symbol algorithm :ironclad))
          (error 'radiance-error :text (format nil "Unknown hashing algorithm configured: ~a" algorithm) :code 21))))

(define-page login #u"auth./login/password" ()
  (let ((user (user:get (server:post "username"))))
    (if (user:saved-p user)
        (let ((model (dm:get-one "linked-passwords" (db:query (:= "username" (username user))))))
          (if model
              (let ((hash (make-password-hash (server:post "password") (dm:field model "salt") (dm:field model "hash-type"))))
                (if (string= hash (dm:field model "hash"))
                    (progn (session:start user)
                           (user:action user "Login (Password)")
                           (server:redirect (get-redirect)))
                    (error 'auth-login-error :text "Invalid username or password." :code 22)))
              (error 'auth-login-error :text "Invalid username or password." :code 21)))
        (error 'auth-login-error :text "Invalid username or password." :code 20))))

(define-page register #u"auth./register/password" ()
  (ignore-errors (auth:authenticate))
  (if (not *radiance-session*) (setf *radiance-session* (session:start-temp)))
  (let ((password (server:post "password"))
        (pwconfirm (server:post "pwconfirm")))
    (if (and password pwconfirm (> (length password) 0))
        (if (string= password pwconfirm)
            (session:field *radiance-session* "password" :value password)
            (error 'auth-register-error :text "Passwords do not match." :code 22))
        (error 'auth-register-error :text "Password not given." :code 23))))

(defmechanism password
    ""
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-password.html"))))
      (if (string= (server:get "mechanism") "password")
          ($ element "#passworderror" (text (server:get "errortext"))))
      element))
  
  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-password.html"))))
      (when (and *radiance-session* (session:field *radiance-session* "password"))
        ($ element "h2" (html "<i class=\"icon-ok-sign\"></i> Password linked."))
        ($ element "input" (remove)))
      element))
  
  (show-options (target)
    (when (string= (server:post "form") "password")
      (setf (config-tree :verify :password :salt) (server:post "salt")
            (config-tree :verify :password :algorithm) (string-upcase (server:post "algorithm")))
      (uibox:notice "Password settings updated."))

    (let ((form (lquery:parse-html (read-data-file "template/verify/admin-auth-password.html"))))
      ($ form "input[name=\"algorithm\"]" 
         (replace-with (uibox:input-select "algorithm" (append (ironclad:list-all-digests) '("PBKDF2")) :selected (config-tree :verify :password :algorithm))))
      ($ form "input[name=\"salt\"]" 
         (val (config-tree :verify :password :salt)))
      (if (config-tree :verify :password :use-per-user-salt)
          ($ form "input[name=\"per-user\"]" 
             (attr :checked "checked")))
      ($ target 
         (append form))))

  (handle-register (user)
    (let ((password (session:field *radiance-session* "password")))
      (if (and password (> (length password) 0))
          (let ((salt (ironclad:byte-array-to-hex-string (radiance-crypto:make-salt T)))
                (model (dm:hull "linked-passwords")))
            (dm:field model "salt" :value salt)
            (dm:field model "hash-type" :value (config-tree :verify :password :algorithm))
            (dm:field model "hash-date" :value (get-unix-time))
            (dm:field model "hash" :value (make-password-hash password salt))
            (dm:field model "username" :value (username user))
            (dm:insert model)
            T)))))
