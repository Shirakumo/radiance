(in-package #:cl-user)
(defpackage #:radiance-staple
  (:nicknames #:org.shirakumo.radiance.staple)
  (:use #:cl)
  (:export #:generate))
(in-package #:org.shirakumo.radiance.staple)

#+quicklisp (ql:quickload :staple-markdown :cl-who)
#-quicklisp (asdf:load-systems :staple-markdown :cl-who)

(defclass api-endpoint (definitions:global-definition definitions:callable) ())

(definitions:define-simple-type-map api-endpoint radiance:api-endpoint)
(definitions:define-simple-object-lookup api-endpoint radiance:api-endpoint)
(definitions:define-simple-documentation-lookup api-endpoint radiance:api-endpoint)
(definitions:define-simple-definition-resolver api-endpoint radiance:api-endpoint)

(defmethod staple:definition-order ((_ api-endpoint))
  91)

(defmethod definitions:arguments ((api-endpoint api-endpoint))
  (radiance:argslist (definitions:object api-endpoint)))

(defclass uri-dispatcher (definitions:global-definition) ())

(definitions:define-simple-type-map uri-dispatcher radiance:uri-dispatcher)
(definitions:define-simple-object-lookup uri-dispatcher radiance:uri-dispatcher)
(definitions:define-simple-documentation-lookup uri-dispatcher radiance:uri-dispatcher)
(definitions:define-simple-definition-resolver uri-dispatcher radiance:uri-dispatcher)

(defmethod staple:definition-order ((_ uri-dispatcher))
  92)

(defclass hook (definitions:global-definition definitions:callable) ())

(definitions:define-simple-type-map hook radiance:hook)
(definitions:define-simple-object-lookup hook (def)
  (radiance:hook (definitions:designator def) (definitions:package def)))
(definitions:define-definition-resolver hook (designator package)
  (when (ignore-errors (radiance:hook designator package))
    (list (make-instance 'hook :designator designator :package package))))

(defmethod staple:definition-order ((_ hook))
  99)

(defmethod definitions:arguments ((hook hook))
  (modularize-hooks:arglist (definitions:object hook)))

(defclass resource-type (definitions:global-definition) ())

(definitions:define-simple-type-map resource-type radiance:resource-type)
(definitions:define-simple-object-lookup resource-type radiance:resource-type)
(definitions:define-simple-documentation-lookup resource-type radiance:resource-type)
(definitions:define-simple-definition-resolver resource-type radiance:resource-type)

(defmethod staple:definition-order ((_ resource-type))
  151)

(defclass option (definitions:global-definition)
  ((type :initarg :type :accessor option-type)))

(definitions:define-simple-type-map option radiance:option)
(definitions:define-simple-object-lookup option (def)
  (radiance:option (option-type def) (definitions:designator def)))
(definitions:define-definition-resolver option (designator package)
  (loop for option in (radiance:list-options designator)
        collect (make-instance 'option :package package
                                       :type designator
                                       :designator (radiance:name option))))

(defmethod staple:definition-order ((_ option))
  152)

(defclass route (definitions:global-definition)
  ((direction :initarg :direction :accessor route-direction)))

(definitions:define-simple-type-map route radiance:route)
(definitions:define-simple-object-lookup route (def)
  (radiance:route (definitions:designator def) (route-direction def)))
(definitions:define-definition-resolver route (designator package)
  (let ((map (radiance:route designator :mapping))
        (rev (radiance:route designator :reversal)))
    (append (when map (list (make-instance 'route :designator designator
                                                  :direction :mapping
                                                  :package package)))
            (when rev (list (make-instance 'route :designator designator
                                                  :direction :reversal
                                                  :package package))))))

(defmethod staple:definition-order ((_ route))
  153)

(defmethod staple:images ((system (eql (asdf:find-system :radiance))))
  (list (asdf:system-relative-pathname :radiance "static/radiance.png")))

(defmethod staple:subsystems ((system (eql (asdf:find-system :radiance))))
  ())

(defmethod staple:packages ((system (eql (asdf:find-system :radiance))))
  (mapcar #'find-package '(:radiance-core
                           :admin
                           :auth
                           :ban
                           :cache
                           :database
                           :relational-database
                           :logger
                           :mail
                           :profile
                           :rate
                           :server
                           :session
                           :user)))
