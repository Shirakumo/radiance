#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:r-ratify
  (:use #:cl #:radiance)
  (:export
   #:session-var
   #:verify-nonce
   #:unacceptable-field
   #:form-field
   #:message
   #:present-error
   #:with-form
   #:define-form-parser
   #:define-form-getter))
(in-package #:r-ratify)

(defun session-var (var &optional (session *session*))
  (session:field session var))

(ratify:define-test user (name)
  (unless (user:get name :if-does-not-exist NIL)
    (ratify:ratification-error name "No user with name ~a found." name)))

(ratify:define-parser user (name)
  (user:get name))

(defun verify-nonce (nonce &key (hash (session-var :nonce-hash)) (salt (session-var :nonce-salt)))
  (if (string= hash (cryptos:pbkdf2-hash nonce salt))
      nonce
      (ratify:ratification-error nonce "Invalid nonce.")))

(defun parse-length (type var length)
  (unless (= (length var) length)
    (ratify:ratification-error var "~s is not ~d characters long." var length))
  (ratify:parse type var))

(defun parse-range (type var min max)
  (unless (<= min (length var) max)
    (ratify:ratification-error var "~s is not between ~d and ~d characters long." var min max))
  (ratify:parse type var))

(define-condition unacceptable-field (error)
  ((form-field :initarg :form-field :initform "???" :accessor form-field)
   (reason :initarg :reason :initform NIL :accessor reason)))

(defgeneric present-error (error)
  (:method ((error null))
    NIL)
  (:method ((error string))
    error)
  (:method ((error error))
    (princ-to-string error))
  (:method ((error unacceptable-field))
    (format NIL "Unacceptable value for the ~a.~@[ <span class=\"reason\">~a</span>~]"
            (form-field error) (present-error (reason error))))
  (:method ((error ratify:ratification-error))
    (format NIL "Reason: ~a"
            (ratify:message error)))
  (:method ((error ratify:test-failed))
    (format NIL "Failed to test for ~a.~@[ ~a~]"
            (ratify:test-name error)
            (present-error (ratify:cause error))))
  (:method ((error ratify:combined-error))
    (format NIL "<ul class=\"errorlist\">~{<li>~a</li>~}</ul>"
            (mapcar #'present-error (ratify:errors error)))))

(defmacro with-field-error ((field) &body body)
  `(ratify:with-skipping
     (handler-case (progn ,@body)
       (error (err)
         (l:error :ARGHGHGNGNGH "~a" err)
         (error 'unacceptable-field
                :form-field ',field
                :reason err)))))

;; spec
;; parse-form ::= (type var*)
;; type       ::= name | (name arg*)
;; var        ::= name | (func name arg*)
(defvar *form-parsers* (make-hash-table))
(defvar *form-getters* (make-hash-table))

(defmacro with-form (parse-forms &body body)
  (flet ((parser (type getter-form)
           (destructuring-bind (type &rest args) (if (listp type) type (list type))
             (let ((parser (gethash type *form-parsers*)))
               (if parser
                   (apply parser getter-form args)
                   (ecase (length args)
                     (0 `(ratify:parse ,type ,getter-form))
                     (1 `(parse-length ,type ,getter-form ,(first args)))
                     (2 `(parse-range ,type ,getter-form ,(first args) ,(second args))))))))
         (getter (var)
           (destructuring-bind (func var &rest args) (if (listp var) var (list :post/get var))
             (let ((getter (gethash func *form-getters*)))
               (if getter
                   (apply getter (string var) args)
                   `(funcall ,func ,(string var) ,@args))))))
    (let* ((enumerated-forms
             (loop for (type . vars) in parse-forms
                   appending (loop for var in vars
                                   collect `(,type ,var)))))
      `(destructuring-bind ,(loop for (type var) in enumerated-forms
                                  collect (if (listp var) (second var) var))
           (ratify:with-errors-combined
             (list
              ,@(loop for (type var) in enumerated-forms
                      collect `(with-field-error (,var)
                                 ,(parser type (getter var))))))
         ,@body))))

(defmacro define-form-getter (name args &body body)
  `(setf (gethash ,(intern (string name) "KEYWORD") *form-getters*)
         #'(lambda ,args ,@body)))

(defmacro define-form-parser (name args &body body)
  `(setf (gethash ,(intern (string name) "KEYWORD") *form-parsers*)
         #'(lambda ,args ,@body)))

(define-form-getter get (var &optional (request '*request*))
  `(get-var ,var ,request))

(define-form-getter post (var &optional (request '*request*))
  `(post-var ,var ,request))

(define-form-getter post/get (var &optional (request '*request*))
  `(post/get ,var ,request))

(define-form-getter session (var &optional (session '*session*))
  `(session:field ,session ,var))

(define-form-parser nonce (getter &optional hash salt)
  `(verify-nonce ,getter ,@(when salt `(:salt ,salt)) ,@(when hash `(:hash ,hash))))
