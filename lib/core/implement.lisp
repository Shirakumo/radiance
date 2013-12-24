#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defmacro define-interface (name &body function-declarations)
  "Define a new implementation mechanism."
  (let ((fqpn (intern (format NIL "ORG.TYMOONNEXT.RADIANCE.INTERFACE.~a" name) :KEYWORD))
        (macro-name (gensym "INTERFACE-GENERATOR"))
        (pkg-impl-var (gensym "PKG-IMPL-VAR"))
        (pkg-impl-fun (gensym "PKG-IMPL-FUN")))
    (flet ((interface-function (funcname args options)
             (let* ((pkg-function (gensym "PKG-FUNCTION"))
                    (pkg-method (gensym "PKG-METHOD"))
                    (pkg-module (gensym "PKG-MODULE"))
                    (restsymb (gensym "REST"))
                    (wholesymb (gensym "WHOLE"))
                    (documentation (second (assoc :documentation options)))
                    (type (second (assoc :type options)))
                    (argsvarlist)
                    (argsgeneric (cons pkg-module (alexandria:flatten args))))

               ;; Add rest and allow-other-keys
               (unless (or (find '&body args) (find '&rest args))
                 (setf args (append args (list '&rest restsymb)))
                 (if (find '&key args)
                     (setf argsgeneric (append argsgeneric '(&allow-other-keys)))
                     (setf argsgeneric (append argsgeneric '(&key &allow-other-keys)))))
               ;; Create pure var list.
               (setf argsvarlist (remove-if #'(lambda (a) (find a '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))
                                            (alexandria:flatten args)))
               
               ;; Triply nested macros. Prepare for hell.               
               `(let ((,pkg-function (find-symbol ,(format NIL "~a" funcname) ',name))
                      (,pkg-method (intern ,(format NIL "I-~a" funcname) ',name)))
                  `(progn
                     (defgeneric ,,pkg-method (,@',argsgeneric))
                     (defmacro ,,pkg-function (&whole ,',wholesymb ,@',args)
                       ,@',(when documentation (list documentation))
                       (declare (ignore ,@',argsvarlist))
                       ,,(ecase type
                                ((:macro 'macro)
                                 ``(apply #',,pkg-method ,,pkg-impl-var ,',wholesymb))
                                ((:function 'function NIL)
                                 ```(apply #',',,pkg-method ,',,pkg-impl-var ,,',wholesymb)))))))))
      
      `(progn
         (defpackage ,fqpn
           (:nicknames ,(intern (string-upcase name) :KEYWORD))
           (:export ,@(append '(#:*implementation* #:implementation) (mapcar #'(lambda (a) (make-symbol (string-upcase (car a)))) function-declarations))))
         (macrolet ((,macro-name ()
                      (let ((,pkg-impl-var (find-symbol "*IMPLEMENTATION*" ',name))
                            (,pkg-impl-fun (find-symbol "IMPLEMENTATION" ',name)))
                        `(progn
                           (defvar ,,pkg-impl-var)
                           (declaim (inline ,,pkg-impl-fun))
                           (defun ,,pkg-impl-fun () ,,pkg-impl-var)
                           ,,@(loop for declaration in function-declarations
                                 collect (destructuring-bind (funcname args &rest options) declaration
                                           (interface-function funcname args options)))))))
           (,macro-name))
         (find-package ',name)))))

(defmacro define-interface-function (function argslist &body body)
  (let ((pkg-method (find-symbol (format NIL "I-~a" function) (symbol-package function))))
    `(defmethod ,pkg-method ,argslist
       ,@body)))
