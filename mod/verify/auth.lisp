#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-verify)

(define-condition auth-session-error (auth-error) ())

(define-interface-method auth:authenticated-p (&key (session *radiance-session*))
  (and session (session:user session) (user:saved-p :user (session:user session)) (session:active-p session)))

(define-interface-method auth:authenticate ()
  (let ((token (server:cookie "token")))
    (when (and token (> (length token) 0) (not *radiance-session*))
      ;; Decrypt token with global key to get user and session data
      (setf token (rad-crypto:decrypt (parse-integer token :radix 36) (config-tree :verify :session :secret))) 
      (unless (and token (find #\- token))
        (error 'auth-session-error :text (format nil "Malformed token: ~a" token) :code 1))
      (let* ((username (subseq token 0 (position #\- token)))
             (user (user:get username))
             (token (subseq token (1+ (position #\- token)))))
        (unless (user:saved-p :user user)
          (error 'auth-session-error :text (format nil "Unknown user: ~a" username) :code 2))
        ;; If per user secrets are activated, decrypt session data with the secret.
        (setf token (if (config-tree :verify :session :use-per-user-secret)
                        (rad-crypto:decrypt token (user:field user "secret"))
                        token))
        (let ((data (split-sequence:split-sequence #\: token)))
          (unless (= (length data) 3)
            (error 'auth-session-error :text (format nil "Invalid data length: ~a" (length data)) :code 4))
          (destructuring-bind (timestamp random session-id) data
            (declare (ignore random))
            (let ((session (session:get session-id)))
              (unless session
                (server:set-cookie "token")
                (error 'auth-session-error :text (format nil "Invalid session: ~a" session-id) :code 5))
              (unless (= (parse-integer timestamp) (session-time session))
                (error 'auth-session-error :text (format nil "Timestamp mismatch: ~a ~a" timestamp (session-time session)) :code 6))
              (v:debug :verify.auth "User ~a successfully authenticated session ~a (initiated on ~a)" user session-id timestamp)
              (setf (s-user session) user)
              (setf *radiance-session* session)
              session)))))))

(define-hook (:interface :profile) (:documentation "Create verify user panel.")
  (eval '(profile:define-panel accounts authentication (:menu-icon "fa-" :lquery (template "verify/user-accounts.html"))
          )))
