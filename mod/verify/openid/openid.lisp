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
    (if *radiance-session*
        (session-field *radiance-session* "nodelete" :value T)
        (setf *radiance-session* (session-start-temp (implementation 'session))))
    (session-field *radiance-session* "link-in-progress" :value :openid)
    (session-field *radiance-session* "relying-party" :value rp)
    (session-field *radiance-session* "redirect" :value (radiance-mod-verify::get-redirect))
    (hunchentoot:redirect
     (cl-openid:initiate-authentication rp (hunchentoot:post-parameter "openid_identifier")))))

(defun handle-response ()
  (if (or (not *radiance-session*) (not (session-temp-p *radiance-session*)))
      (error 'auth-login-error :text "No temporary session active!" :code 12))
  (let ((rp (session-field *radiance-session* "relying-party")))
    (session-field *radiance-session* "link-in-progress" :value NIL)
    (unless (session-field *radiance-session* "nodelete")
        (session-end *radiance-session*))
    (cl-openid:handle-indirect-response 
     rp 
     (hunchentoot:get-parameters *radiance-request*)
     (puri:merge-uris (hunchentoot:request-uri *radiance-request*) (cl-openid:root-uri rp)))))

(defun process-response (authproc)
  (log:debug "Claimed ID: ~a" (cl-openid:claimed-id authproc))
  (let ((map (model-get-one (implementation 'data-model) "linked-openids" 
                            (:= "claimed-id" (format nil "~a" (cl-openid:claimed-id authproc))))))
    (if map
        (user-get (implementation 'user) (model-field map "username"))
        (error 'auth-login-error :text "Account not linked!" :code 13))))

(defvar *response-processor* #'process-response)

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
                 (funcall *response-processor* authproc)
                 (error 'auth-login-error :text "Authentication failed!" :code 11)))
         (cl-openid:openid-assertion-error (err)
           (error 'auth-login-error :text err :code 12))))
      
      (T (error 'auth-login-error :text "No ID given!" :code 10))))

  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-openid.html"))))
      (when *radiance-session*
        (loop for provider in ($ element (find "li"))
           do (loop for link in (session-field *radiance-session* "openid-links")
                 do (if (search (first ($ provider (attr :title))) link :test #'string-equal)
                        ($ provider (add-class "linked"))))))
      element))
  
  (handle-link ()
    (let ((*response-processor* (lambda (authproc)
                                  (let ((id (format nil "~a" (cl-openid:claimed-id authproc))))
                                    (log:debug "Linking: ~a" id)
                                    (nappend (session-field *radiance-session* "openid-links") (list id))))))
      (when (or (string= (hunchentoot:post-parameter "openid") "Link") (eq (session-field *radiance-session* "link-in-progress") :openid))
        (handle-login (radiance-mod-verify::get-mechanism :openid)))))
  
  (handle-register (user)
    (let ((links (session-field *radiance-session* "openid-links")))
      (loop with db = (implementation 'database)
         for link in links
         do (db-insert db "linked-openids" 
                       (acons "claimed-id" link
                       (acons "username" (user-field user "username") 
                       ())))))))
