#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-openid)
(use-package :radiance-mod-verify)

(defmechanism openid
    ""
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-openid.html"))))
      (if (string= (hunchentoot:get-parameter "mechanism") "openid")
          ($ element "#openiderror" (text (hunchentoot:get-parameter "errortext"))))
      element))

  (handle-login ()
    (error 'auth-login-error :text "FOOBAR!" :code 666)))
