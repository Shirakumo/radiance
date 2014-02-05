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
    (v:info :radiance.server.hunchentoot "Starting listener ~a on ~:[localhost~;~:*~a~]:~a" name address port)
    (setf (gethash name *listeners*) listener)
    (hunchentoot:start listener)))

(define-interface-method server:stop-listener (name)
  (assert (not (null (gethash name *listeners*))) () "No listener ~a known!" name)
  (v:info :radiance.server.hunchentoot "Stopping listener ~a" name)
  (hunchentoot:stop (gethash name *listeners*))
  (remhash name *listeners*))

(define-interface-method server:get-listeners ()
  (alexandria:hash-table-keys *listeners*))

(define-interface-method server:cookie (name &key (request *radiance-request*))
  (hunchentoot:cookie-in name request))

(defmethod (setf server::i-cookie) (value (module (eql :radiance-hunchentoot)) name)
  (server::i-set-cookie :radiance-hunchentoot name :value value))

(define-interface-method server:cookies (&key (request *radiance-request*))
  (hunchentoot:cookies-in request))

(define-interface-method server:get (name &key (request *radiance-request*))
  (declare (optimize (speed 3)) (string name))
  (if (and (> (length name) 2) (string= name "[]" :start1 (- (length name) 2)))
      (assoc-all name (server:gets :request request) :val #'cdr :test #'string=)
      (hunchentoot:get-parameter name request)))

(defmethod (setf server::i-get) (value (module (eql :radiance-hunchentoot)) name)
  (setf (cdr (assoc name (hunchentoot:get-parameters *radiance-request*) :test #'string=)) name))

(define-interface-method server:gets (&key (request *radiance-request*))
  (hunchentoot:get-parameters request))

(define-interface-method server:post (name &key (request *radiance-request*))
  (declare (optimize (speed 3)) (string name))
  (if (and (> (length name) 2) (string= name "[]" :start1 (- (length name) 2)))
      (assoc-all name (server:posts :request request) :val #'cdr :test #'string=)
      (hunchentoot:post-parameter name request)))

(defmethod (setf server::i-post) (value (module (eql :radiance-hunchentoot)) name)
  (setf (cdr (assoc name (hunchentoot:post-parameters *radiance-request*) :test #'string=)) name))

(define-interface-method server:posts (&key (request *radiance-request*))
  (hunchentoot:post-parameters request))

(define-interface-method server:post-or-get (name &key (request *radiance-request*))
  (or (server::i-post :radiance-hunchentoot name :request request)
      (server::i-get :radiance-hunchentoot name :request request)))

(defmethod (setf server::i-post-or-get) (value (module (eql :radiance-hunchentoot)) name)
  (if (server::i-post :radiance-hunchentoot name)
      (setf (server::i-post :radiance-hunchentoot name) value)
      (setf (server::i-get :radiance-hunchentoot name) value)))

(define-interface-method server:header (name &key (request-or-response *radiance-request*))
  (etypecase request-or-response
    (server:request (hunchentoot:header-in name request-or-response))
    (server:response (hunchentoot:header-out name request-or-response))))

(defmethod (setf server::i-header) (value (module (eql :radiance-hunchentoot)) name)
  (server::i-set-header :radiance-hunchentoot name value))

(define-interface-method server:headers (&key (request-or-response *radiance-request*))
  (etypecase request-or-response
    (server:request (hunchentoot:headers-in request-or-response))
    (server:response (hunchentoot:headers-out request-or-response))))

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

(define-interface-method server:content-type (&key (response *radiance-response*))
  (hunchentoot:content-type response))

(define-interface-method server:set-cookie (name &key (value "") domain (path "/") (expires (+ (get-universal-time) *default-cookie-expire*)) (http-only T) secure (response *radiance-response*))
  (flet ((setc (domain) (hunchentoot:set-cookie name :value value :domain domain :path path :expires expires :http-only http-only :secure secure :reply response)))
    (v:debug :radiance.server.hunchentoot "Setting cookie '~a' on ~a ~a exp ~a (HTTP ~a;SECURE ~a) to ~a" name domain path expires http-only secure value)
    (if domain
        (setc domain)
        (setc (format NIL ".~a" (domain *radiance-request*))))))

(define-interface-method server:set-default-content-type (content-type)
  (v:debug :radiance.server.hunchentoot "Setting default content-type to: ~a" content-type)
  (setf hunchentoot:*default-content-type* content-type))

(define-interface-method server:set-content-type (content-type &key (response *radiance-response*))
  (v:debug :radiance.server.hunchentoot "Setting content-type to: ~a" content-type)
  (setf (hunchentoot:content-type* response) content-type))

(define-interface-method server:set-return-code (return-code &key (response *radiance-response*))
  (v:debug :radiance.server.hunchentoot "Setting return-code to: ~a" return-code)
  (setf (hunchentoot:return-code response) return-code))

(define-interface-method server:set-header (name value &key (response *radiance-response*))
  (setf (hunchentoot:header-out name response) value))

(define-interface-method server:set-response-content (content &key (response *radiance-response*))
  (setf (server::content response) content))

(define-interface-method server:redirect ((uri T) &key (response *radiance-response*))
  (v:debug :radiance.server.hunchentoot "Redirecting to ~a" uri)
  (let ((hunchentoot:*reply* response))
    (hunchentoot:redirect (string uri))))

(define-interface-method server:redirect ((uri uri) &key (response *radiance-response*))
  (server::i-redirect :radiance-hunchentoot (uri->url uri) :response response))

(define-interface-method server:uploaded-file (post-parameter &key (request *radiance-request*))
  (let ((param (server::i-post :radiance-hunchentoot post-parameter :request request)))
    (assert (listp param) (param) "Post parameter does not contain a file!")
    (assert (not (not param)) (param) "Post parameter does not exist!")
    (destructuring-bind (tempfile origname mimetype) param
      (values tempfile origname mimetype (file-size tempfile)))))

(define-interface-method server:serve-file (pathname &key content-type (response *radiance-response*))
  (v:debug :radiance.server.hunchentoot "Serving file from ~a (~a)" pathname content-type)
  (let ((hunchentoot:*reply* response))
    (if content-type
        (hunchentoot:handle-static-file pathname content-type)
        (hunchentoot:handle-static-file pathname))))

(define-interface-method server:set-handler-function (handler-fun)
  (v:debug :radiance.server.hunchentoot "Setting handler function to: ~a" handler-fun)
  (setf *handler* handler-fun))

(defmacro with-* (interface function)
  (let ((varsgens (gensym "VARS")) (vargens (gensym "VAR")) (bodygens (gensym "BODY")))
    `(define-interface-method ,interface (,varsgens &body ,bodygens)
       `(symbol-macrolet ,(loop for ,vargens in ,varsgens
                                collect `(,,vargens (,',function :radiance-hunchentoot ,(string-downcase ,vargens))))
          ,@,bodygens))))

(with-* server:with-gets server::i-get)
(with-* server:with-posts server::i-post)
(with-* server:with-posts-or-gets server::i-post-or-get)
(with-* server:with-headers server::i-header)
(with-* server:with-cookies server::i-cookie)

(define-hook (:server :init) (:documentation "Set up hunchentoot.")
  (setf hunchentoot:*dispatch-table* (list #'pre-handler)))

(defun pre-handler (request)
  (declare (ignore request))
  (parse-request hunchentoot:*request*)
  (funcall *handler* hunchentoot:*request* hunchentoot:*reply*)
  (lambda () (server::content hunchentoot:*reply*)))
