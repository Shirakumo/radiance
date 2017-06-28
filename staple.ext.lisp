(in-package #:cl-user)
(defpackage #:radiance-staple
  (:nicknames #:org.shirakumo.radiance.staple)
  (:use #:cl)
  (:export #:generate))
(in-package #:org.shirakumo.radiance.staple)

(defclass symb-api-endpoint (staple:symb-function)
  ())

(defmethod staple:symb-function ((symb symb-api-endpoint))
  (radiance::handler (radiance:api-endpoint (staple:symb-symbol symb))))

(defmethod staple:symb-documentation ((symb symb-api-endpoint))
  (documentation (staple:symb-symbol symb) 'radiance:api-endpoint))

(defmethod staple:symb-type-order ((symb (eql 'symb-api-endpoint)))
  (+ 1 (staple:symb-type-order 'staple:symb-function)))

(staple:define-simple-converter symb-api-endpoint radiance:api-endpoint)

(defclass symb-uri-dispatcher (staple:symb-function)
  ())

(defmethod staple:symb-function ((symb symb-uri-dispatcher))
  (radiance::dispatch-function (radiance:uri-dispatcher (staple:symb-symbol symb))))

(defmethod staple:symb-documentation ((symb symb-uri-dispatcher))
  (documentation (staple:symb-symbol symb) 'radiance:uri-dispatcher))

(defmethod staple:symb-type-order ((symb (eql 'symb-uri-dispatcher)))
  (+ 2 (staple:symb-type-order 'staple:symb-function)))

(staple:define-simple-converter symb-uri-dispatcher radiance:uri-dispatcher)

(defclass symb-resource-type (staple:symb-class)
  ())

(defmethod staple:symb-documentation ((symb symb-resource-type))
  (documentation (staple:symb-symbol symb) 'radiance:resource-type))

(defmethod staple:symb-type-order ((symb (eql 'symb-resource-type)))
  (+ 1 (staple:symb-type-order 'staple:symb-class)))

(staple:define-converter symb-resource-type (symbol package)
  (let ((type (ignore-errors (radiance:resource-type symbol))))
    (when type (list (make-instance 'symb-resource-type :symbol symbol
                                                        :package package)))))

(defclass symb-option (staple:symb-class)
  ((type :initarg :type :accessor option-type)))

(defmethod staple:symb-documentation ((symb symb-option))
  (documentation (radiance:option (option-type symb) (staple:symb-symbol symb)) T))

(defmethod staple:symb-type-order ((symb (eql 'symb-option)))
  (+ 2 (staple:symb-type-order 'staple:symb-class)))

(defmethod staple:symb-type ((symb symb-option))
  (format NIL "option ~a" (option-type symb)))

(staple:define-converter symb-option (type package)
  (loop for option in (radiance:list-options type)
        collect (make-instance 'symb-option
                               :package package
                               :symbol (radiance:name option)
                               :type type)))

(defclass symb-route (staple:symb-class)
  ((direction :initarg :direction :accessor route-direction)))

(defmethod staple:symb-documentation ((symb symb-route))
  (documentation (radiance:route (staple:symb-symbol symb) (route-direction symb)) T))

(defmethod staple:symb-type-order ((symb (eql 'symb-route)))
  (+ 3 (staple:symb-type-order 'staple:symb-class)))

(staple:define-converter symb-route (name package)
  (let ((map (radiance:route name :mapping))
        (rev (radiance:route name :reversal)))
    (append (when map (list (make-instance 'symb-route :symbol name
                                                       :direction :mapping
                                                       :package package)))
            (when rev (list (make-instance 'symb-route :symbol name
                                                       :direction :reversal
                                                       :package package))))))

(setf (staple:system-packages :radiance) '(:radiance-core
                                           :admin
                                           :auth
                                           :ban
                                           :cache
                                           :database
                                           :logger
                                           :profile
                                           :rate
                                           :server
                                           :session
                                           :user))

(defmethod staple:system-options append ((system (eql (asdf:find-system 'radiance))))
  (list :if-exists :supersede
        :logo "static/radiance.png"))
