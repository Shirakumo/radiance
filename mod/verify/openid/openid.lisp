#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-openid)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :radiance-mod-verify :radiance-mod-verify-openid))

(defun get-relying-party ()
  (let* ((subdomains (subdomains *radiance-request*))
         (domain (domain *radiance-request*))
         (port (port *radiance-request*))
         (path (path *radiance-request*))
         (realm (format NIL "http://~{~a.~}~a:~a/" subdomains domain port)))
    (make-instance 'cl-openid:relying-party
                   :root-uri (puri:merge-uris path realm)
                   :realm (puri:uri realm))))

(defun handle-initiate ()
  (let ((rp (get-relying-party))
        (session (session-start-temp (implementation 'session))))
    (session-field session "relying-party" :value rp)
    (hunchentoot:redirect
     (cl-openid:initiate-authentication rp (hunchentoot:post-parameter "openid_identifier")))))

(defun handle-response ()
  (if (or (not *radiance-session*) (not (session-temp-p *radiance-session*)))
      (error 'auth-login-error :text "No temporary session active!" :code 12))
  (let ((rp (session-field *radiance-session* "relying-party")))
    (session-end *radiance-session*)
    (cl-openid:handle-indirect-response 
     rp 
     (hunchentoot:get-parameters *radiance-request*)
     (puri:merge-uris (hunchentoot:request-uri *radiance-request*) (cl-openid:root-uri rp)))))

(defun process-response (authproc)
  (log:debug "Claimed ID: ~a" (cl-openid:claimed-id authproc))
  (let ((map (model-get-one (implementation 'data-model) "linked-openids" 
                            (query (= "claimed-id" (format nil "~a" (cl-openid:claimed-id authproc)))))))
    (if map
        (user-get (implementation 'user) (model-field map "username"))
        (error 'auth-login-error :text "Account not linked!" :code 13))))

(defmechanism openid
    "Mechanism for OpenID-Supporting sites."
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-openid.html"))))
      (if (string= (hunchentoot:get-parameter "mechanism") "openid")
          ($ element "#openiderror" (text (hunchentoot:get-parameter "errortext"))))
      element))

  (handle-login ()
    (cond
      ((hunchentoot:post-parameter "openid_identifier")
       (handle-initiate))
      
      ((hunchentoot:get-parameter cl-openid:+authproc-handle-parameter+)
       (handler-case
           (multiple-value-bind (id authproc) (handle-response)
             (if id
                 (process-response authproc)
                 (error 'auth-login-error :text "Authentication failed!" :code 11)))
         (cl-openid:openid-assertion-error (err)
           (error 'auth-login-error :text err :code 12))))
      
      (T (error 'auth-login-error :text "No ID given!" :code 10)))))
