#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defun make-password-hash (user password)
  ""
  (let ((salt (if (config-tree :verify :password :use-per-user-salt)
                  (user-field user "salt")
                  (config-tree :verify :password :salt)))
        (algorithm (or (user-field user "hash-type")
                       (setf algorithm (config-tree :verify :password :algorithm)))))
    (if (string-equal algorithm "pbkdf2")
        (pbkdf2-hash password salt)
        (if (find-symbol algorithm :ironclad)
            (simple-hash password salt :digest (find-symbol algorithm :ironclad))
            (error 'radiance-error :text (format nil "Unknown hashing algorithm configured: ~a" algorithm) :code 21)))))

(defmechanism password
    ""
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-password.html"))))
      (if (string= (hunchentoot:get-parameter "mechanism") "password")
          ($ element "#passworderror" (text (hunchentoot:get-parameter "errortext"))))
      element))

  (handle-login ()
    (let ((user (user-get (implementation 'user) (hunchentoot:post-parameter "username"))))
      (if (user-saved-p user)
          (let ((hash (make-password-hash user (hunchentoot:post-parameter "password"))))
            (if (string= hash (user-field user "hash"))
                user
                (error 'auth-login-error :text "Invalid username or password." :code 20)))
          (error 'auth-login-error :text "Invalid username or password." :code 20))))
  
  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-password.html")))
          (post-data (session-field *radiance-session* "post-data")))
      (when (and *radiance-session* post-data)
        ($ element "#password-in-1" (val (cdr (assoc "password" post-data :test #'string=))))
        ($ element "#password-in-2" (val (cdr (assoc "pwconfirm" post-data :test #'string=)))))
      element))

  (handle-link () NIL)

  (handle-register (user)
    (let ((password (hunchentoot:post-parameter "password" *radiance-request*))
          (pwconfirm (hunchentoot:post-parameter "pwconfirm" *radiance-request*)))
      (if (and password pwconfirm (> (length password) 0))
          (if (string= password pwconfirm)
              (let ((salt (make-salt T)))
                (user-field user "salt" :value salt)
                (user-field user "hash-type" :value (config-tree :verify :password :algorithm))
                (user-field user "hash-date" :value (get-unix-time))
                (user-field user "hash" :value (make-password-hash user password)))
              (error 'auth-register-error :text "Passwords to not match!" :code 22))))))
