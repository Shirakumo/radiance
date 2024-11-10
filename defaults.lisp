(in-package #:org.shirakumo.radiance.core)

;; Sets up a default trigger for pages
(define-option page :hook (name body uri &optional (value T))
  (declare (ignore uri))
  (assert (symbolp value))
  (if value
      (let ((name (if (eql value T) name value)))
        (values
         `((trigger '(,name ,(package-name *package*)))
           ,@body)
         `(define-hook (,name ,(package-name *package*)) ())))
      body))

(define-option page :uri-groups (name body uri &optional uri-groups)
  (declare (ignore name))
  (if uri-groups
      `((cl-ppcre:register-groups-bind ,uri-groups (,(path uri) (path (uri *request*)))
          ,@body))
      body))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (defun transform-access-body (body branch type)
    (if (not (eql branch T))
        `((unless (and (auth:current)
                       ,@(when branch `((user:check (auth:current) ,branch))))
            (error ',type :message (format NIL "~a does not have access~@[ to ~s~]"
                                           (user:username (or (auth:current) (user:get "anonymous"))) ,branch)))
          ,@body)
        body)))

(define-option page :access (name body uri &optional (branch T))
  (declare (ignore name uri))
  (transform-access-body body branch 'request-denied))

(define-option api :access (name body args &optional (branch T))
  (declare (ignore name args))
  (transform-access-body body branch 'api-auth-error))

(define-option admin:panel :access (name body category &optional (branch T))
  (declare (ignore name category))
  (transform-access-body body branch 'request-denied))

(define-option profile:panel :access (name body &optional (branch T))
  (declare (ignore name))
  (transform-access-body body branch 'request-denied))

;; Standard serialisations
(defmethod api-serialize ((error radiance-condition))
  (let ((table (make-hash-table)))
    (flet ((s (k v) (setf (gethash (string-downcase k) table) v)))
      (s :error-type (type-of error))
      (s :message (message error))
      (when (typep error 'request-error)
        (s :uri (uri-to-url (uri (slot-value error 'request)) :representation :external)))
      (when (typep error '(or api-argument-missing
                              api-argument-invalid))
        (s :argument (slot-value error 'argument)))
      (when (typep error 'api-unknown-format)
        (s :format (slot-value error 'format))))
    table))

;; Api catchall page
(define-api || () ()
  (error 'api-call-not-found))

;; Api standard format
(define-api-format lisp (object)
  (when (boundp '*response*)
    (setf (content-type *response*) "text/x-sexpr"))
  (with-output-to-string (stream)
    (macrolet ((w (&rest args)
                 `(progn ,@(loop for arg in args
                                 collect (etypecase arg
                                           (string `(write-string ,arg stream))
                                           (character `(write-char ,arg stream))
                                           (list arg))))))
      (labels ((output (object)
                 (typecase object
                   ((or string symbol number)
                    (write object :stream stream))
                   (list
                    (w "("
                       (loop (typecase (cdr object)
                               (cons
                                (w (output (car object)) " ")
                                (setf object (cdr object)))
                               (null
                                (w (output (car object)))
                                (return))
                               (T (w (output (car object)) " . " (output (cdr object)))
                                (return))))
                       ")"))
                   (vector
                    (w "#("
                       (when (< 0 (length object))
                         (output (aref object 0))
                         (loop for i from 1 below (length object)
                               do (w " " (output (aref object i)))))
                       ")"))
                   (hash-table
                    (w "#{"
                       (loop for i downfrom (hash-table-count object)
                             for k being the hash-keys of object
                             for v being the hash-values of object
                             do (w (output k) ": " (output v))
                                (unless (= i 1) (w ", ")))
                       "}"))
                   (T (output (api-serialize object))))))
        (output object)))))

(unless (boundp '*default-api-format*)
  (setf *default-api-format* "lisp"))

;; Default urls
(define-page favicon ("/^favicon.ico$" 10) ()
  (serve-file (static-file "favicon.ico" :radiance-core)))

(define-page robots ("/^robots.txt$" 10) ()
  (serve-file (static-file "robots.txt" :radiance-core)))

(define-page static ("/^static/.*" 1000) ()
  (let* ((path (path (uri *request*)))
         (slashpos (position #\/ path :start (length "static/")))
         (file (if slashpos
                   (ignore-errors
                    (static-file (parse-path-safely (subseq path (1+ slashpos))) (string-upcase (subseq path (length "static/") slashpos))))
                   (static-file (parse-path-safely (subseq path (length "static/"))) :radiance-core))))
    (if (and file (uiop:file-exists-p file))
        (serve-file file)
        (error 'request-not-found))))

(defvar *config-routes* ())
(define-trigger (environment-change 'compile-config-routes) ()
  (loop for (name direction) in *config-routes*
        do (remove-route name direction))
  (setf *config-routes* NIL)
  (loop for (name direction from to priority) in (config :routes)
        do (with-simple-restart (continue "Ignore the route definition.")
             (funcall (ecompile NIL `(lambda () (define-string-route ,name (,direction ,(or priority 0)) ,from ,to))))
             (push (list name direction) *config-routes*))))

;; Default routing to cut domains.
(defvar *domain-internalizers* ())

(defun add-domain (domain)
  (pushnew domain (config :domains) :test #'string-equal)
  (setf *domain-internalizers* (compile-domain-internalizers))
  (config :domains))

(defun remove-domain (domain)
  (setf (config :domains) (remove domain (config :domains) :test #'string-equal))
  (setf *domain-internalizers* (compile-domain-internalizers))
  (config :domains))

(defun compile-domain-internalizers (&optional (domains (config :domains)))
  (list*
   (lambda (uri)
     (when (loop for domain in (domains uri)
                 always (cl-ppcre:scan "^\\d+$" domain))
       (values (format NIL "~{~a~^.~}" (reverse (domains uri))) ())))
   (loop for domain in domains
         collect (let ((parts (nreverse (cl-ppcre:split "\\." domain)))
                       (domain domain))
                   (lambda (uri)
                     (loop for subdomains = (domains uri) then (cdr subdomains)
                           for a = (car subdomains)
                           for b in parts
                           when (or (not subdomains) (not (string-equal a b)))
                           do (return NIL)
                           finally (return (values domain subdomains))))))))

(define-trigger (environment-change 'compile-domain-internalizers) ()
  (setf *domain-internalizers* (compile-domain-internalizers)))

(define-route domain (:mapping most-positive-fixnum) (uri)
  (loop for internalizer in *domain-internalizers*
        until (multiple-value-bind (domain subdomains) (funcall internalizer uri)
                (when domain
                  (when (boundp '*request*)
                    (setf (domain *request*) domain))
                  (setf (domains uri) subdomains)
                  T))))

(define-route domain (:reversal most-negative-fixnum) (uri)
  (cond
    ((boundp '*request*)
     (push (domain *request*) (domains uri))
     (unless (port uri)
       (setf (port uri) (port (uri *request*)))))
    ;; KLUDGE! Find a better way to do this...
    (T
     (push (first (config :domains)) (domains uri))
     (unless (port uri)
       (setf (port uri) (config :port))))))

(define-route virtual-module (:mapping 100000) (uri)
  (when (and (< 1 (length (path uri)))
             (string= "!/" (path uri) :end2 2))
    (let ((slashpos (position #\/ (path uri) :start 2)))
      (let ((module (subseq (path uri) 2 slashpos))
            (path (if slashpos
                      (subseq (path uri) (1+ slashpos))
                      "")))
        (when (boundp '*request*)
          (setf (gethash 'virtual-module (data *request*)) module))
        (setf (path uri) path)
        (setf (domains uri) (append (domains uri) (list module)))))))

(define-route virtual-module (:reversal -100000) (uri)
  (when (boundp '*request*)
    (let ((virtual (gethash 'virtual-module (data *request*)))
          (last (car (last (domains uri)))))
      (when virtual
        ;; Check if we are operating on the requested virtual domain
        (cond ((string-equal virtual last)
               (setf (path uri) (format NIL "!/~a/~a" virtual (path uri))
                     (domains uri) (butlast (domains uri))))
              (last
               ;; Otherwise we need to use the specified subdomain as virtual
               ;; since we're trying to cross-reference a resource.
               (setf (path uri) (format NIL "!/~a/~a" last (path uri))
                     (domains uri) (butlast (domains uri))))
              ((starts-with "api/" (path uri))
               ;; API requests need to retain the virtual module in case of
               ;; redirects back.
               (setf (path uri) (format NIL "!/~a/~a" virtual (path uri))))
              (T
               ;; Other refer, no change.
               ))))))

;;; Default logger to make sure we can log even before the real impl is loaded.
(unless (implementation 'logger)
  (setf (fdefinition 'l:log)
        (lambda (level category log-string &rest format-args)
          (if (stringp log-string)
              (format *error-output* "~&~a [~a] <~a> ~?~%"
                      (format-human-date (get-universal-time)) level category log-string format-args)
              (format *error-output* "~&~a [~a] <~a> ~a~%"
                      (format-human-date (get-universal-time)) level category log-string))))

  (setf (fdefinition 'l:trace)
        (lambda (category log-string &rest format-args)
          (declare (ignore category log-string format-args))))

  (setf (fdefinition 'l:debug)
        (lambda (category log-string &rest format-args)
          (declare (ignore category log-string format-args))))

  (setf (fdefinition 'l:info)
        (lambda (category log-string &rest format-args)
          (apply #'l:log :info category log-string format-args)))

  (setf (fdefinition 'l:warn)
        (lambda (category log-string &rest format-args)
          (apply #'l:log :warn category log-string format-args)))

  (setf (fdefinition 'l:error)
        (lambda (category log-string &rest format-args)
          (apply #'l:log :error category log-string format-args)))

  (setf (fdefinition 'l:severe)
        (lambda (category log-string &rest format-args)
          (apply #'l:log :severe category log-string format-args)))

  (setf (fdefinition 'l:fatal)
        (lambda (category log-string &rest format-args)
          (apply #'l:log :fatal category log-string format-args))))
