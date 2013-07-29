#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(define-condition radiance-error (error)
  ((text :initarg :text :initform "Undefined Error")
   (code :initarg :code :initform -1))
  (:report (lambda (c s) (format s "~a: ~a (E~4d)" 
                                 (class-name (class-of c))
                                 (slot-value c 'text)
                                 (slot-value c 'code)))))

(define-condition auth-error (radiance-error) ())

(define-condition api-error (radiance-error)
  ((module :initarg :module :initform NIL)
   (apicall :initarg :apicall :initform NIL))
  (:report (lambda (c s) (format s "~a: ~a/~a: ~a" 
                                 (class-name (class-of c)) 
                                 (class-name (class-of (slot-value c 'module))) 
                                 (slot-value c 'apicall)
                                 (slot-value c 'text)))))

(define-condition api-args-error (api-error) ())
