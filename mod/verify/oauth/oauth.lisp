#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify-oauth)

(define-hook (:server :init) (:documentation "Initialize verify-oauth table.")
  (db:create "linked-oauths" '(("provider" :varchar 32) ("claimed-id" :varchar 128) ("username" :varchar 32))))

(defun get-callback ()
  (uri->url *radiance-request*))

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
  (let* ((rt (get-request-token provider))
         (link (cl-oauth:make-authorization-uri (config-tree :verify :oauth provider :auth-endpoint) rt :callback-uri (get-callback))))
    (session:field *radiance-session* "redirect" :value (server:referer))
    (session:field *radiance-session* "request-token" :value rt)
    (session:field *radiance-session* "provider" :value provider)
    (v:debug :verify.mechanism.oauth "Initiating OAuth handle: ~a" link)
    (server:redirect (format nil "~a" link))))

(defun handle-response ()
  (if (or (not *radiance-session*) (not (session:temp-p *radiance-session*)))
      (error 'auth-login-error :text "No temporary session active!" :code 12))
  (let ((rt (session:field *radiance-session* "request-token"))
        (provider (session:field *radiance-session* "provider")))
    (session:field *radiance-session* "link-in-progress" :value NIL)
    (assert rt)
    (handler-case
        (cl-oauth:authorize-request-token-from-request
         (lambda (rt-key)
           (assert (equal (cl-oauth:url-encode rt-key) (cl-oauth:token-key rt)) () "Keys do not match!")
           rt))
      (error (err)
        (error 'auth-login-error :text (format nil "Failed to verify: ~a" err) :code 13)))
    (when (cl-oauth:request-token-authorized-p rt)
      (v:debug :verify.mechanism.oauth "Successfully authorized with token ~a" (cl-oauth:token-key rt))
      (process-response provider (get-access-token provider rt)))))

(defgeneric process-response (provider access-token))
(defmethod process-response ((provider (eql :twitter)) access-token)
  (let* ((data (babel:octets-to-string (cl-oauth:access-protected-resource "http://api.twitter.com/1.1/account/verify_credentials.json" access-token)))
         (credentials (json:decode-json-from-string data)))
    (v:debug :verify.mechanism.oauth "Claimed ID: ~a" (cdr (assoc :id credentials)))
    (values (format nil "~a" (cdr (assoc :id credentials))) "twitter")))

(defun get-linked-user (id provider)
  (let ((model (dm:get-one "linked-oauths" (db:query (:and (:= "claimed-id" id) (:= "provider" provider))))))
    (if model
        (user:get (dm:field model "username"))
        (error 'auth-login-error :text "Account not linked!" :code 15))))

(core:define-page login #u"auth./login/oauth" ()
  (ignore-errors (auth:authenticate))
  (if (not *radiance-session*) (setf *radiance-session* (session:start-temp)))
  (cond
    ((server:post "provider")
     (let ((provider (make-keyword (server:post "provider"))))
       (if (config-tree :verify :oauth provider)
           (handle-initiate provider)
           (error 'auth-login-error :text "Unknown provider!" :code 11))))
    
    ((and *radiance-session* (session:field *radiance-session* "request-token"))
     (multiple-value-bind (id provider) (handle-response)
       (session:end *radiance-session*)
       (let ((user (get-linked-user id provider)))
         (session:start user)
         (user:action "Login (OAuth)" :user user))))
    
    (T (error 'auth-login-error :text "Nothing to do!" :code 10))))

(core:define-page register #u"auth./register/oauth" ()
  (ignore-errors (auth:authenticate))
  (if (not *radiance-session*) (setf *radiance-session* (session:start-temp)))
  (cond
    ((server:post "provider")
     (let ((provider (make-keyword (server:post "provider"))))
       (if (config-tree :verify :oauth provider)
           (handle-initiate provider)
           (error 'auth-register-error :text "Unknown provider!" :code 11))))
    
    ((and *radiance-session* (session:field *radiance-session* "request-token"))
     (multiple-value-bind (id provider) (handle-response)
       (v:debug :verify.mechanism.oauth "Linking: ~a/~a" id provider)
       (appendf (getdf *radiance-session* "oauth-links") (list (cons provider id)))))

    (T (error 'auth-register-error :text "Nothing to do!" :code 10))))
    

(defmechanism oauth
    "Mechanism for OpenID-Supporting sites."
  (show-login ()
    (lquery:parse-html (read-data-file "template/verify/login-oauth.html")))

  (show-register ()
    (let ((element (lquery:parse-html (read-data-file "template/verify/register-oauth.html"))))
      (when *radiance-session*
        (loop for link in (session:field *radiance-session* "oauth-links")
           do ($ element (find (format nil "li.~a" (car link))) (add-class "linked")))
        (if (> (length (session:field *radiance-session* "oauth-links")) 0)
            ($ element (find "h2") (html "<i class=\"icon-ok-sign\"></i> Account linked."))))
      element))
  
  (show-options (target)
    (when (string= (server:post "form") "oauth")
      (loop for name in (server:post "name[]")
         for key in (server:post "key[]")
         for secret in (server:post "secret[]")
         for request in (server:post "request[]")
         for auth in (server:post "auth[]")
         for access in (server:post "access[]")
         do (setf (config-tree :verify :oauth (make-keyword name))
                  `((:key . ,key) (:secret . ,secret)
                    (:request-endpoint . ,request) (:auth-endpoint . ,auth) (:access-endpoint . ,access))))
      (uibox:notice "OAuth providers updated."))

    (let ((form (lquery:parse-html (read-data-file "template/verify/admin-auth-oauth.html"))))
      (loop with template = ($ form "#providers li" (node))
            for node = (dom:clone-node template T)
            for (name . vals) in (config-tree :verify :oauth)
            do
               ($ node "h4" (text name))
               ($ node "input[name=\"name[]\"]" (val name))
               ($ node "input[name=\"key[]\"]" (val (cdr (assoc :key vals))))
               ($ node "input[name=\"secret[]\"]" (val (cdr (assoc :secret vals))))
               ($ node "input[name=\"request[]\"]" (val (cdr (assoc :request-endpoint vals))))
               ($ node "input[name=\"auth[]\"]" (val (cdr (assoc :auth-endpoint vals))))
               ($ node "input[name=\"access[]\"]" (val (cdr (assoc :access-endpoint vals))))
            collect node into nodes
            finally ($ form "#providers" (empty) (append nodes)))
      ($ target 
        (append form))))
  
  (handle-register (user)
    (let ((links (session:field *radiance-session* "oauth-links")))
      (loop for link in links
            do (db:insert "linked-oauths" 
                          (acons "provider" (car link)
                                 (acons "claimed-id" (cdr link)
                                        (acons "username" (user:field user "username") 
                                               ())))))
      (if links T))))
