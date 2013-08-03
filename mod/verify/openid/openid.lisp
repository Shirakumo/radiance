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
  (let ((rp (get-relying-party)))
    (session-field *radiance-session* "relying-party" :value rp)
    (session-field *radiance-session* "redirect" :value (radiance-mod-verify::get-redirect))
    (redirect (cl-openid:initiate-authentication rp (post-var "openid_identifier")))))

(defun handle-response ()
  (if (or (not *radiance-session*) (not (session-temp-p *radiance-session*)))
      (error 'auth-login-error :text "No temporary session active!" :code 12))
  (let ((rp (session-field *radiance-session* "relying-party")))
    (cl-openid:handle-indirect-response 
     rp 
     (get-vars)
     (puri:merge-uris (hunchentoot:request-uri *radiance-request*) (cl-openid:root-uri rp)))))

(defpage login #u"auth./login/openid" ()
  (ignore-errors (authenticate T))
  (if (not *radiance-session*) (setf *radiance-session* (session-start-temp T)))
  (cond
    ((post-var "openid_identifier")
     (handle-initiate))
    
    ((get-var cl-openid:+authproc-handle-parameter+)
     (handler-case
         (multiple-value-bind (id authproc) (handle-response)
           (if id
               (let ((map (model-get-one T "linked-openids" (:= "claimed-id" (format nil "~a" (cl-openid:claimed-id authproc))))))
                 (if map
                     (progn 
                       (session-end *radiance-session*)
                       (session-start T (user-get (implementation 'user) (model-field map "username"))))
                     (error 'auth-login-error :text "Account not linked!" :code 13)))
               (error 'auth-login-error :text "Authentication failed!" :code 11)))
       (cl-openid:openid-assertion-error (err)
         (error 'auth-login-error :text err :code 12))))
    
    (T (error 'auth-login-error :text "No ID given!" :code 10))))

(defpage register #u"auth./register/openid" ()
  (ignore-errors (authenticate T))
  (if (not *radiance-session*) (setf *radiance-session* (session-start-temp T)))
  (cond
    ((post-var "openid_identifier")
     (handle-initiate))
    
    ((get-var cl-openid:+authproc-handle-parameter+)
       (handler-case
           (multiple-value-bind (id authproc) (handle-response)
             (if id
                 (let ((id (format nil "~a" (cl-openid:claimed-id authproc))))
                   (log:debug "Linking: ~a" id)
                   (nappend (session-field *radiance-session* "openid-links") (list id)))
                 (error 'auth-register-error :text "Authentication failed!" :code 11)))
         (cl-openid:openid-assertion-error (err)
           (error 'auth-register-error :text err :code 12))))
    
    (T (error 'auth-register-error :text "No ID given!" :code 10))))


(defmechanism openid
    "Mechanism for OpenID-Supporting sites."
  (show-login ()
    (lquery:parse-html (read-data-file "template/verify/login-openid.html")))

  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-openid.html"))))
      (when *radiance-session*
        (loop for link in (session-field *radiance-session* "openid-links")
           do (loop for provider in ($ element (find "li"))
                 do (if (search (first ($ provider (attr :title))) link :test #'string-equal)
                        ($ provider (add-class "linked")))))
        (if (> (length (session-field *radiance-session* "openid-links")) 0)
            ($ element (find "h2") (html "<i class=\"icon-ok-sign\"></i> Account linked."))))
      element))
  
  (handle-register (user)
    (let ((links (session-field *radiance-session* "openid-links")))
      (loop with db = (implementation 'database)
         for link in links
         do (db-insert db "linked-openids" 
                       (acons "claimed-id" link
                       (acons "username" (user-field user "username") 
                       ())))))))
