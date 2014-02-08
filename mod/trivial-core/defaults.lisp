#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(core::m-define-file-link :trivial-core favicon #u"/favicon.ico" (static "img/favicon.ico") :content-type "image/x-icon")
(core::m-define-file-link :trivial-core robots #u"/robots.txt" (static "txt/robots.txt") :content-type "text/plain")
(core::m-define-file-link :trivial-core humans #u"/humans.txt" (static "txt/humans.txt") :content-type "text/plain")

(core::m-define-api :trivial-core formats () (:method :GET)
  "Lists all the available API output formats."
  (core::i-api-return :trivial-core 200 "Available output formats" :data (alexandria:hash-table-keys *radiance-api-formats*)))

(core::m-define-api :trivial-core version () (:method :GET)
  "Show the current framework version."
  (core::i-api-return :trivial-core 200 "Radiance Version" :data (asdf:component-version (context-module))))

(core::m-define-api :trivial-core host () (:method :GET)
  "Lists information about the host machine."
  (core::i-api-return :trivial-core 200 "Host information" :data
                      (plist->hash-table
                       :machine-instance (machine-instance)
                       :machine-type (machine-type)
                       :machine-version (machine-version)
                       :software-type (software-type)
                       :software-version (software-version)
                       :lisp-implementation-type (lisp-implementation-type)
                       :lisp-implementation-version (lisp-implementation-version))))

(core::m-define-api :trivial-core modules () (:method :GET)
  "Lists the currently loaded radiance modules."
  (core::i-api-return :trivial-core 200 "Module listing" :data *radiance-modules*))

(core::m-define-api :trivial-core server () (:method :GET)
  "Returns information about the radiance server."
  (core::i-api-return :trivial-core 200 "Server information" :data
                      (plist->hash-table
                       :string (format nil "TyNET-~a-SBCL~a-α" (asdf:component-version (context-module)) (lisp-implementation-version))
                       :ports (config :ports)
                       :uptime (- (get-unix-time) *radiance-startup-time*)
                       :request-count *radiance-request-count*
                       :request-total *radiance-request-total*)))

(core::m-define-api :trivial-core noop () (:method :GET)
  "Returns a NOOP page.")

(core::m-define-api :trivial-core echo () (:method T)
  "Returns the map of POST and GET data sent to the server."
  (core::i-api-return :trivial-core 200 "Echo data" :data (list :post (server:posts) :get (server:gets))))

(core::m-define-api :trivial-core user () (:method :GET)
  "Shows data about the current user."
  (core::i-api-return :trivial-core 200 "User data" :data
                      (plist->hash-table
                       :authenticated (auth:authenticated-p)
                       :session-active (if *radiance-session* T NIL))))

(core::m-define-api :trivial-core error () (:method :GET)
  "Generates an api-error page."
  (error 'api-error :text "Api error as requested" :code -42))

(core::m-define-api :trivial-core internal-error () (:method :GET)
  "Generates an internal-error page."
  (error 'radiance-error :text "Internal error as requested" :code -42))

(core::m-define-api :trivial-core unexpected-error () (:method :GET)
  "Generates an unexpected error page."
  (error "Unexpected error as requested"))

(core::m-define-api :trivial-core coffee () (:method :GET)
  "RFC-2324"
  (core::i-api-return :trivial-core 418 "I'm a teapot." :data
                      (plist->hash-table
                       :temperature (+ 65 (random 20))
                       :active T
                       :capacity 1
                       :content (/ (+ (random 60) 40) 100)
                       :flavour (random-elt '("rose hip" "peppermint" "english breakfast" "green tea" "roiboos"))
                       :additives (random-elt '("none" "none" "none" "none" "none" "sugar" "sugar" "sugar" "lemon" "cream" "milk")))))

(core::m-define-api :trivial-core request () (:method :GET)
  "Returns information about the current request."
  (with-slots (subdomains domain port path) *radiance-request*
    (core::i-api-return :trivial-core 200 "Request data" :data
                        (plist->hash-table
                         :subdomains subdomains
                         :domain domain
                         :port port
                         :path path
                         :remote-addr (server:remote-address)
                         :remote-port (server:remote-port)
                         :referer (server:referer)
                         :method (server:request-method)
                         :post (server:posts)
                         :get (server:gets)
                         :cookie (server:cookies)
                         :header (server:headers)))))

(core::m-define-api :trivial-core continuations () (:method :GET :access-branch "*")
  "Shows information about continuations for the current user."
  (core::i-api-return :trivial-core 200 "Active continuations" :data
                      (mapcar #'(lambda (cont)
                                  (plist->hash-table
                                   :id (id cont)
                                   :name (name cont)
                                   :timeout (timeout cont)
                                   :request (format NIL "~a" (request cont))))
                              (continuations))))

(core::m-define-api :trivial-core index () (:method :GET)
  "Returns a map of all possible API calls and their docstring."
  (core::i-api-return :trivial-core 200 "Api call index" :data
                      (let ((table (make-hash-table)))
                        (mapc #'(lambda (item-name)
                                  (setf (gethash item-name table)
                                        (mapcar #'(lambda (item)
                                                    (multiple-value-bind (identifier method) (identifier-and-method (item-identifier item))
                                                      (plist->hash-table
                                                       :method (if (string-equal "T" method) "ANY" method)
                                                       :module identifier
                                                       :description (item-documentation item))))
                                                (hook-items :api item-name))))
                              (hooks :api))
                        table)))
