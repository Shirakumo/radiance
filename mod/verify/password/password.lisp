#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-password)

(defmethod init-pass-db ((module verify-password))
  (db-create T "linked-passwords" '(("salt" :varchar 32) ("hash-type" :varchar 16) ("hash-date" :integer) ("hash" :varchar 128) ("username" :varchar 32))))
(defhook :server :init (get-module :verify-password) #'init-pass-db)

(defun make-password-hash (password &optional (salt (config-tree :verify :password :salt)) (algorithm (config-tree :verify :password :algorithm)))
  ""
  (if (string-equal algorithm "pbkdf2")
      (pbkdf2-hash password salt)
      (if (find-symbol algorithm :ironclad)
          (simple-hash password salt :digest (find-symbol algorithm :ironclad))
          (error 'radiance-error :text (format nil "Unknown hashing algorithm configured: ~a" algorithm) :code 21))))

(defpage login #u"auth./login/password" ()
  (let ((user (user-get T (post-var "username"))))
    (if (user-saved-p user)
        (let ((model (model-get-one T "linked-passwords" (query (:= "username" (username user))))))
          (if model
              (let ((hash (make-password-hash (post-var "password") (model-field model "salt") (model-field model "hash-type"))))
                (if (string= hash (model-field model "hash"))
                    (progn (session-start T user)
                           (user-action user "Login (Password)")
                           (redirect (get-redirect)))
                    (error 'auth-login-error :text "Invalid username or password." :code 22)))
              (error 'auth-login-error :text "Invalid username or password." :code 21)))
        (error 'auth-login-error :text "Invalid username or password." :code 20))))

(defpage register #u"auth./register/password" ()
  (ignore-errors (authenticate T))
  (if (not *radiance-session*) (setf *radiance-session* (session-start-temp T)))
  (let ((password (post-var "password" *radiance-request*))
        (pwconfirm (post-var "pwconfirm" *radiance-request*)))
    (if (and password pwconfirm (> (length password) 0))
        (if (string= password pwconfirm)
            (session-field *radiance-session* "password" :value password)
            (error 'auth-register-error :text "Passwords do not match." :code 22))
        (error 'auth-register-error :text "Password not given." :code 23))))

(defmechanism password
    ""
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-password.html"))))
      (if (string= (get-var "mechanism") "password")
          ($ element "#passworderror" (text (get-var "errortext"))))
      element))
  
  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-password.html"))))
      (when (and *radiance-session* (session-field *radiance-session* "password"))
        ($ element "h2" (html "<i class=\"icon-ok-sign\"></i> Password linked."))
        ($ element "input" (remove)))
      element))
  
  (show-options (target)
    (when (string= (post-var "form") "password")
      (setf (config-tree :verify :password :salt) (post-var "salt")
            (config-tree :verify :password :algorithm) (string-upcase (post-var "algorithm")))
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
    (let ((password (session-field *radiance-session* "password")))
      (if (and password (> (length password) 0))
          (let ((salt (ironclad:byte-array-to-hex-string (make-salt T)))
                (model (model-hull T "linked-passwords")))
            (model-field model "salt" :value salt)
            (model-field model "hash-type" :value (config-tree :verify :password :algorithm))
            (model-field model "hash-date" :value (get-unix-time))
            (model-field model "hash" :value (make-password-hash password salt))
            (model-field model "username" :value (username user))
            (model-insert model)
            T)))))
