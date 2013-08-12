#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defpage api #u"/api/" (:modulevar module)
  (let ((pathparts (split-sequence:split-sequence #\/ (path *radiance-request*)))
        (format (make-keyword (or (get-var "format") (post-var "format") "json"))))
    (api-format 
     format
     (handler-case 
         (case (length pathparts)
           ((1 2) (api-return 200 (format NIL "Radiance API v~a" (version module))
                              (plist->hash-table :VERSION (version module) :TIME (get-unix-time))))
           (otherwise
            (let* ((module (cadr pathparts))
                   (trigger (make-keyword (concatenate-strings (cddr pathparts) "/")))
                   (hooks (get-hooks :api trigger)))
              (or (call-api module hooks)
                  (api-return 204 "No return data")))))
       (api-args-error (c)
         (api-return 400 "Invalid arguments"
                     (plist->hash-table :errortype (class-name (class-of c)) 
                                        :code (slot-value c 'code)
                                        :text (slot-value c 'text))))
       (api-error (c)
         (api-return 500 "Api error"
                     (plist->hash-table :errortype (class-name (class-of c))
                                        :code (slot-value c 'code)
                                        :text (slot-value c 'text))))))))
(defun call-api (module hooks)
  (loop with return = ()
     with accepted = NIL
     for hook in hooks
     if (string-equal (class-name (class-of (module hook))) module)
     do (setf accepted T)
       (nappend return (funcall (hook-function hook) (module hook) (request-method)))
     finally (return (if accepted
                         return
                         (api-return 404 "Call not found")))))

(define-api-format json "application/json" data
  (cl-json:encode-json-to-string data))

(defapi formats () ()
  (api-return 200 "Available output formats" (alexandria:hash-table-keys *radiance-api-formats*)))

(defapi version () (:modulevar module)
  (api-return 200 "Radiance Version" (version module)))

(defapi host () ()
  (api-return 200 "Host information" 
              (plist->hash-table
               :machine-instance (machine-instance)
               :machine-type (machine-type)
               :machine-version (machine-version)
               :software-type (software-type)
               :software-version (software-version)
               :lisp-implementation-type (lisp-implementation-type)
               :lisp-implementation-version (lisp-implementation-version))))

(defapi modules () ()
  (api-return 200 "Module listing"
              (alexandria:hash-table-keys *radiance-modules*)))

(defapi server () (:modulevar module)
  (api-return 200 "Server information"
              (plist->hash-table
               :string (format nil "TyNET-~a-SBCL~a-α" (version module) (lisp-implementation-version))
               :ports (config :ports)
               :uptime (- (get-unix-time) *radiance-startup-time*)
               :time (get-unix-time)
               :request-count *radiance-request-count*
               :request-total *radiance-request-total*)))

(defapi noop () ())

(defapi echo () ()
  (api-return 200 "Echo data" (list :post (post-vars) :get (get-vars))))

(defapi user () ()
  (api-return 200 "User data"
              (plist->hash-table
               :authenticated (authenticated-p)
               :session-active (if *radiance-session* T NIL))))

(defapi error () ()
  (error 'api-error :text "Api error as requested" :code -42))

(defapi coffee () ()
  (api-return 418 "I'm a teapot"
              (plist->hash-table
               :temperature (+ 75 (random 10))
               :active T
               :capacity 1
               :content (/ (+ (random 20) 80) 100)
               :flavour (alexandria:random-elt '("rose hip" "peppermint" "english breakfast")))))

(defapi request () ()
  (with-slots (subdomains domain port path) *radiance-request*
    (api-return 200 "Request data"
                (plist->hash-table
                 :subdomains subdomains
                 :domain domain
                 :port port
                 :path path
                 :remote-addr (hunchentoot:remote-addr *radiance-request*)
                 :remote-port (hunchentoot:remote-port *radiance-request*)
                 :referer (hunchentoot:referer *radiance-request*)
                 :method (hunchentoot:request-method *radiance-request*)
                 :post (post-vars)
                 :get (get-vars)
                 :cookie (cookie-vars)
                 :header (header-vars)))))
