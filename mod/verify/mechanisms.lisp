#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(defmechanism password
    ""
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-password.html"))))
      (if (string= (hunchentoot:get-parameter "mechanism") "password")
          ($ element "#passworderror" (text (hunchentoot:get-parameter "errortext"))))
      element))

  (handle-login ()
    (error 'auth-login-error :text "FOOBAR!" :code 666))
  
  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-password.html")))
          (post-data (session-field *radiance-session* "post-data")))
      (when (and *radiance-session* post-data)
        ($ element "#password-in-1" (val (cdr (assoc "password" post-data :test #'string=))))
        ($ element "#password-in-2" (val (cdr (assoc "pwconfirm" post-data :test #'string=)))))
      element))

  (handle-link () NIL)

  (handle-register () NIL))
