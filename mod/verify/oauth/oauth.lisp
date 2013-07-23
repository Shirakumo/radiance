#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-oauth)
(eval-when (:compile-toplevel :load-toplevel :execute)
  (use-package :radiance-mod-verify :radiance-mod-verify-oauth))

(defun get-callback ()
  (format NIL "http://~{~a.~}~a:~a~a"
          (subdomains *radiance-request*)
          (domain *radiance-request*)
          (port *radiance-request*)
          (path *radiance-request*)))

(defun get-request-token (provider)
  (cl-oauth:obtain-request-token
   (config-tree :verify :oauth provider :request-endpoint)
   (cl-oauth:make-consumer-token :key (config-tree :verify :oauth provider :key)
                                 :secret (config-tree :verify :oauth provider :secret))
   :callback-uri (get-callback)))

(defun get-access-token (provider request-token)
  (cl-oauth:obtain-access-token 
   (config-tree :verify :oauth provider :access-endpoint)
   request-token))

(defun handle-initiate (provider)
  (let ((rt (get-request-token provider)))
    (if *radiance-session*
        (session-field *radiance-session* "nodelete" :value T)
        (setf *radiance-session* (session-start-temp (implementation 'session))))
    (session-field *radiance-session* "redirect" :value (radiance-mod-verify::get-redirect))
    (session-field *radiance-session* "link-in-progress" :value :oauth)
    (session-field *radiance-session* "request-token" :value rt)
    (session-field *radiance-session* "provider" :value provider)
    (hunchentoot:redirect
     (format nil "~a" (cl-oauth:make-authorization-uri (config-tree :verify :oauth provider :auth-endpoint) rt :callback-uri ())))))

(defun handle-response ()
  (if (or (not *radiance-session*) (not (session-temp-p *radiance-session*)))
      (error 'auth-login-error :text "No temporary session active!" :code 12))
  (let ((rt (session-field *radiance-session* "request-token"))
        (provider (session-field *radiance-session* "provider")))
    (session-field *radiance-session* "link-in-progress" :value NIL)
    (unless (session-field *radiance-session* "nodelete")
        (session-end *radiance-session*))
    (assert rt)
    (handler-case
        (cl-oauth:authorize-request-token-from-request
         (lambda (rt-key)
           (assert (equal (cl-oauth:url-encode rt-key) (cl-oauth:token-key rt)) () "Keys do not match!")
           rt))
      (error (err)
        (error 'auth-login-error :text (format nil "Failed to verify: ~a" err) :code 13)))
    (when (cl-oauth:request-token-authorized-p rt)
      (log:debug "Successfully authorized with token ~a" (cl-oauth:token-key rt))
      (process-response provider (get-access-token provider rt)))))

(defgeneric process-response (provider access-token))
(defmethod process-response ((provider (eql :twitter)) access-token)
  (let* ((data (babel:octets-to-string (cl-oauth:access-protected-resource "http://api.twitter.com/1.1/account/verify_credentials.json" access-token)))
         (credentials (json:decode-json-from-string data)))
    (log:debug "Claimed ID: ~a" (cdr (assoc :id credentials)))
    (funcall *response-processor* (format nil "~a" (cdr (assoc :id credentials))) "twitter")))

(defun get-linked-user (id provider)
  (let ((model (model-get-one (implementation 'data-model) "linked-oauths" 
                              (:and (:= "claimed-id" id) (:= "provider" provider)))))
      (if model
          (user-get (implementation 'user) (model-field model "username"))
          (error 'auth-login-error :text "Account not linked!" :code 15))))

(defparameter *response-processor* #'get-linked-user)

(defmechanism oauth
    "Mechanism for OpenID-Supporting sites."
  (show-login ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/login-oauth.html"))))
      (if (string= (hunchentoot:get-parameter "mechanism") "oauth")
          ($ element "#oautherror" (text (hunchentoot:get-parameter "errortext"))))
      element))

  (handle-login ()
    (cond
      ((hunchentoot:post-parameter "provider" *radiance-request*)
       (let ((provider (make-keyword (hunchentoot:post-parameter "provider" *radiance-request*))))
         (if (config-tree :verify :oauth provider)
             (handle-initiate provider)
             (error 'auth-login-error :text "Unknown provider!" :code 11))))
      ((and *radiance-session* (session-field *radiance-session* "request-token"))
       (handle-response))
      (T (error 'auth-login-error :text "Nothing to do!" :code 10))))

  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-oauth.html")))
          (post-data (session-field *radiance-session* "post-data")))
      (when (and *radiance-session* post-data)
        (loop for link in (session-field *radiance-session* "oauth-links")
           do ($ element (find (format nil "li.~a" (car link))) (add-class "linked")))
        )
      element))
  
  (handle-link ()
    (let ((*response-processor* (lambda (id provider)
                                  (log:debug "Linking: ~a/~a" id provider)
                                  (nappend (session-field *radiance-session* "oauth-links") (list (cons provider id))))))
      (when (or (string= (hunchentoot:post-parameter "oauth") "Link") (eq (session-field *radiance-session* "link-in-progress") :oauth))
        (handle-login (radiance-mod-verify::get-mechanism :oauth)))))
  
  (handle-register (user)
    (let ((links (session-field *radiance-session* "oauth-links")))
      (loop with db = (implementation 'database)
         for link in links
         do (db-insert db "linked-oauths" 
                       (acons "provider" (car link)
                       (acons "claimed-id" (cdr link)
                       (acons "username" (user-field user "username") 
                       ()))))))))
