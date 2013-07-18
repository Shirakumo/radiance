#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-oauth)
(use-package :radiance-mod-verify :radiance-mod-verify-oauth)

(defun get-callback ()
  (format NIL "http://~{~a.~}~a:~a/auth/oauth" (subdomains *radiance-request*) (domain *radiance-request*) (port *radiance-request*)))

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
  (let ((rt (get-request-token provider))
        (session (session-start-temp (implementation 'session))))
    (session-field session "request-token" :value rt)
    (session-field session "provider" :value provider)
    (hunchentoot:redirect
     (format nil "~a" (cl-oauth:make-authorization-uri (config-tree :verify :oauth provider :auth-endpoint) rt :callback-uri ())))))

(defun handle-response ()
  (if (or (not *radiance-session*) (not (session-temp-p *radiance-session*)))
      (error 'auth-login-error :text "No temporary session active!" :code 12))
  (let ((rt (session-field *radiance-session* "request-token"))
        (provider (session-field *radiance-session* "provider")))
    (session-end *radiance-session*)
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
    (let ((model (model-get-one (implementation 'data-model) "linked-oauths" 
                                (query (= "claimed-id" (format nil "~a" (cdr (assoc :id credentials)))) (= "provider" "twitter")))))
      (if model
          (user-get (implementation 'user) (model-field model "username"))
          (error 'auth-login-error :text "Account not linked!" :code 15)))))

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
      (T (error 'auth-login-error :text "Nothing to do!" :code 10)))))
