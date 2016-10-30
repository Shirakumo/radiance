#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

;; To be specced
(define-interface ban
  (defun jail (ip &key duration))
  (defun list ())
  (defun jail-time (&optional (ip (remote *request*))))
  (defun release (ip)))

;; To be specced
(define-interface rate
  (defmacro define-rate (name (time-left &key (timeout 60) (limit 1)) &body on-limit-exceeded))
  (defun rate-left (rate &key (ip (remote *request*))))
  (defmacro with-rate-limitation ((rate) &body body)))

;; To be specced
(define-interface admin
  (define-resource-locator page (name &rest args))
  (defun panel (category name))
  (defun (setf panel) (function category name))
  (defun remove-panel (category name))
  (defmacro define-panel-option (name (categoryvar namevar bodyvar valuevar) &body body))
  (defmacro define-panel (name category options &body body)))

;; To be specced
(define-interface cache
  (defun get (name))
  (defun renew (name))
  (defmacro with-cache (name-form test-form &body request-generator)))

;; To be specced
(define-interface auth
  (defvar *login-timeout* (* 60 60 24 365))
  (define-resource-locator page (name &rest args))
  (defun current (&optional (session *session*)))
  (defun associate (user &optional (session *session*)))
  (defun login! (&optional (landing-page (referer *request*))))
  (define-hook associate (session)))

;; To be specced
(define-interface session
  (defvar *default-timeout* (* 60 60 24))
  (defclass session () ())
  (defun = (session-a session-b))
  (defun start ())
  (defun get (session-id))
  (defun list ())
  (defun id (session))
  (defun field (session field))
  (defun (setf field) (value session field))
  (defun timeout (session))
  (defun (setf timeout) (seconds session))
  (defun end (session))
  (defun active-p (session))
  (define-hook create (session)))

;; To be specced
(define-interface user
  (defclass user () ())
  (defun = (user-a user-b))
  (defun list ())
  (defun get (username &key (if-does-not-exist NIL)))
  (defun username (user))
  (defun fields (user))
  (defun field (user field))
  (defun (setf field) (value user field))
  (defun save (user))
  (defun saved-p (user))
  (defun discard (user))
  (defun remove (user))
  (defun check (user branch))
  (defun grant (user branch))
  (defun prohibit (user branch))
  (defun add-default-permission (branch))
  (defun action (user action public))
  (defun actions (user n &key (public T) oldest-first))
  (define-hook remove (username))
  (define-hook action (user action public)))

;; To be specced
(define-interface profile
  (define-resource-locator page (user &optional tab))
  (defun avatar (user size))
  (defun name (user))
  (defun fields ())
  (defun add-field (name &key (type :text) default (editable T)))
  (defun panel (name))
  (defun (setf panel) (function name))
  (defun remove-panel (name))
  (defmacro define-panel-option (name (namevar bodyvar valuevar) &body body))
  (defmacro define-panel (name options &body body)))

;; To be specced
(define-interface server
  (defun start (port &key address ssl-cert ssl-key ssl-pass))
  (defun stop (port &optional address))
  (defun listeners ())
  (define-hook-switch started stopped (port &optional address)))

;; To be specced
(define-interface (logger l)
  (defun log (level category log-string &rest format-args))
  (defun trace (category log-string &rest format-args))
  (defun debug (category log-string &rest format-args))
  (defun info (category log-string &rest format-args))
  (defun warn (category log-string &rest format-args))
  (defun error (category log-string &rest format-args))
  (defun severe (category log-string &rest format-args))
  (defun fatal (category log-string &rest format-args)))

;; As per spec (needs updating)
(define-interface (database db)
  (defun connect (database-name))
  (defun disconnect ())
  (defun connected-p ())
  (defun collections ())
  (defun collection-exists-p (collection))
  (defun create (collection structure &key indices (if-exists :ignore)))
  (defun structure (collection))
  (defun empty (collection))
  (defun drop (collection))
  (defun iterate (collection query function &key fields (skip 0) amount sort accumulate))
  (defun select (collection query &key fields (skip 0) amount sort))
  (defun count (collection query))
  (defun insert (collection data))
  (defun remove (collection query &key (skip 0) amount sort))
  (defun update (collection query data &key (skip 0) amount sort))
  (defmacro with-transaction (() &body body))
  (defmacro query (query-form))
  (define-hook-switch connected disconnected (name)))

;; As per spec
(define-interface (data-model dm)
  (defclass data-model () ())
  (defun id (data-model))
  (defun collection (data-model))
  (defun fields (data-model))
  (defun field (data-model field))
  (defun (setf field) (value data-model field))
  (defun get (collection query &key (skip 0) (amount 0) sort))
  (defun get-one (collection query &key (skip 0) sort))
  (defun hull (collection))
  (defun hull-p (data-model))
  (defun save (data-model))
  (defun delete (data-model))
  (defun insert (data-model &key clone)))
