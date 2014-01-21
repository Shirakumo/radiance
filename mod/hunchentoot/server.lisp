#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-hunchentoot)

(defvar *handler* #'radiance:handler)
(defvar *listeners* (make-hash-table))

(define-interface-method server:start-listener (name &key address (port 80))
  (assert (null (gethash name *listeners*)) () "Listener ~a already started!" name)
  (let ((listener (make-instance 'hunchentoot:easy-acceptor
                                 :port port :address address
                                 :access-log-destination NIL
                                 :message-log-destination NIL
                                 :request-class 'request
                                 :reply-class 'response)))
    (setf (gethash name *listeners*) listener)
    (hunchentoot:start listener)))

(define-interface-method server:stop-listener (name)
  (assert (not (null (gethash name *listeners*))) () "No listener ~a known!" name)
  (hunchentoot:stop (gethash name *listeners*))
  (remhash name *listeners*))

(define-interface-method server:get-listeners ()
  (alexandria:hash-table-keys *listeners*))

(define-interface-method server:cookie (name &key (request *radiance-request*))
  (hunchentoot:cookie-in name request))

(define-interface-method server:cookies (&key (request *radiance-request*))
  (hunchentoot:cookies-in request))

(define-interface-method server:get (name &key (request *radiance-request*))
  (declare (optimize (speed 3)) (string name))
  (if (and (> (length name) 2) (string= name "[]" :start1 (- (length name) 2)))
      (assoc-all name (server:gets :request request) :val #'cdr :test #'string=)
      (hunchentoot:get-parameter name request)))

(define-interface-method server:gets (&key (request *radiance-request*))
  (hunchentoot:get-parameters request))

(define-interface-method server:post (name &key (request *radiance-request*))
  (declare (optimize (speed 3)) (string name))
  (if (and (> (length name) 2) (string= name "[]" :start1 (- (length name) 2)))
      (assoc-all name (server:posts :request request) :val #'cdr :test #'string=)
      (hunchentoot:post-parameter name request)))

(define-interface-method server:posts (&key (request *radiance-request*))
  (hunchentoot:post-parameters request))

(define-interface-method server:post-or-get (name &key (request *radiance-request*))
  (or (server:post name request)
      (server:get name request)))

(define-interface-method server:header (name &key (request *radiance-request*))
  (hunchentoot:header-in name request))

(define-interface-method server:headers (&key (request *radiance-request*))
  (hunchentoot:headers-in request))

(define-interface-method server:request-method (&key (request *radiance-request*))
  (hunchentoot:request-method request))

(define-interface-method server:remote-address (&key (request *radiance-request*))
  (hunchentoot:remote-addr request))

(define-interface-method server:remote-port (&key (request *radiance-request*))
  (hunchentoot:remote-port request))

(define-interface-method server:referer (&key (request *radiance-request*))
  (hunchentoot:referer request))

(define-interface-method server:user-agent (&key (request *radiance-request*))
  (hunchentoot:user-agent request))

(define-interface-method server:local-address (&key (request *radiance-request*))
  (hunchentoot:local-addr request))

(define-interface-method server:local-port (&key (request *radiance-request*))
  (hunchentoot:local-port request))

(define-interface-method server:request-uri (&key (request *radiance-request*))
  (hunchentoot:request-uri request))

(define-interface-method server:set-cookie (name &key (value "") domain (path "/") (expires (+ (get-universal-time) *default-cookie-expire*)) (http-only T) secure (response *radiance-response*))
  (flet ((setc (domain) (hunchentoot:set-cookie name :value value :domain domain :path path :expires expires :http-only http-only :secure secure :response response)))
    (v:debug :radiance.server.request "Setting cookie '~a' on ~a ~a exp ~a (HTTP ~a;SECURE ~a) to ~a" name domain path expires http-only secure value)
    (if domain
        (setc domain)
        (setc (format NIL ".~a" (domain *radiance-request*))))))

(define-interface-method server:set-default-content-type (content-type)
  (setf hunchentoot:*default-content-type* content-type))

(define-interface-method server:set-content-type (content-type &key (response *radiance-response*))
  (v:debug :radiance.server.request "Setting content-type to: ~a" content-type)
  (setf (hunchentoot:content-type* response) content-type))

(define-interface-method server:set-return-code (return-code &key (response *radiance-response*))
  (setf (hunchentoot:return-code response) return-code))

(define-interface-method server:set-header (name value &key (response *radiance-response*))
  (setf (hunchentoot:header-out name response) value))

(define-interface-method server:set-response-content (content &key (response *radiance-response*))
  (setf (body response) content))

(define-interface-method server:redirect ((uri T) &key response)
  (v:debug :radiance.server.request "Redirecting to ~a" uri)
  (let ((hunchentoot:*reply* response))
    (hunchentoot:redirect (string uri))))

(define-interface-method server:redirect ((uri uri) &key (response *radiance-response*))
  (server::i-redirect :radiance-hunchentoot (uri->url uri) :response response))

(define-interface-method server:serve-file (pathname &key content-type response)
  (let ((hunchentoot:*reply* response))
    (if content-type
        (hunchentoot:handle-static-file pathname content-type)
        (hunchentoot:handle-static-file pathname))))

(define-interface-method server:set-handler-function (handler-fun)
  (setf *handler* handler-fun))

(define-hook (:server :init) (:documentation "Set up hunchentoot.")
  (setf hunchentoot:*dispatch-table* (list #'pre-handler)))

(defun pre-handler ()
  (parse-request hunchentoot:*request*)
  (funcall *handler* hunchentoot:*request* hunchentoot:*reply*)
  (lambda () (body hunchentoot:*reply*)))
