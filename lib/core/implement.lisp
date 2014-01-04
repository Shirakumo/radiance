#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defun lambda-keyword-p (symbol)
  (find symbol '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))

(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  (remove-if #'lambda-keyword-p (flatten-lambda-list lambda-list)))

(defun extract-macro-lambda-vars (macro-lambda-list)
  (loop with varlist = ()
     for i from 0 below (length macro-lambda-list)
     for arg in macro-lambda-list
     do (when (find arg '(&key &optional &rest &body &aux))
          (return (append varlist (extract-lambda-vars (nthcdr i macro-lambda-list)))))
       (unless (lambda-keyword-p arg)
         (if (listp arg)
             (appendf varlist (extract-lambda-vars arg))
             (appendf varlist (list arg))))
     finally (return varlist)))

(defun make-key-extensible (generic-lambda-list)
  (if (or (find '&rest generic-lambda-list)
          (find '&body generic-lambda-list))
      generic-lambda-list
      (if (find '&key generic-lambda-list)
          (append generic-lambda-list '(&allow-other-keys))
          (append generic-lambda-list '(&key &allow-other-keys)))))

(defun make-rest-swallowing (lambda-list rest-var)
  (if (or (find '&rest lambda-list)
          (find '&body lambda-list))
      lambda-list
      (let ((keypos (position '&key lambda-list)))
        (if keypos
            (append (subseq lambda-list 0 keypos)
                    (list '&rest rest-var)
                    (subseq lambda-list keypos))
            (append lambda-list (list '&rest rest-var))))))

(defun macro-lambda-list->generic-list (macro-lambda-list)
  (loop with in-required-args = T
     for arg in macro-lambda-list
     collect (cond
               ((or (eql arg '&rest) (eql arg '&key) (eql arg '&optional))
                (setf in-required-args NIL)
                arg)
               ((and in-required-args (listp arg))
                (gensym (format NIL "~{~a~^-~}" (extract-lambda-vars arg))))
               ((listp arg)
                (car arg))
               (T arg))))

(defmacro define-interface (name &body function-declarations)
  "Define a new implementation mechanism."
  (with-gensyms
      ((macro-name    "INTERFACE-GENERATOR")
       (pkg-impl-var  "PKG-IMPL-VAR") (pkg-impl-fun  "PKG-IMPL-FUN")
       (pkg-function  "PKG-FUNCTION") (pkg-method    "PKG-METHOD")
       (new-impl-gens "NEW-IMPL") (provided-gens "PROVIDED"))
    (let ((fqpn (intern (format NIL "ORG.TYMOONNEXT.RADIANCE.INTERFACE.~a" name) :KEYWORD)))
      (flet ((interface-function (funcname args options)
               (let ((documentation (second (assoc :documentation options)))
                     (type (second (assoc :type options)))
                     (wholesymb (gensym "WHOLE"))
                     (argsgeneric (make-key-extensible args)))

                 ;; Fix up generic args for macro-lambda-lists.
                 (if (or (eql type :macro) (eql type 'macro))
                     (progn
                       (setf argsgeneric (macro-lambda-list->generic-list argsgeneric))
                       (let ((bodypos (position '&body argsgeneric)))
                         (when bodypos (setf (nth bodypos argsgeneric) '&rest))))
                     (setf argsgeneric (flatten-lambda-list argsgeneric)))

                 ;; Add rest parameter to allow for additional keyword arguments.
                 (setf args (make-rest-swallowing args (gensym "REST")))
                 
                 ;; Triply nested macros. Woeyy.
                 `(let ((,pkg-function (find-symbol ,(format NIL "~a" funcname) ',name))
                        (,pkg-method (intern ,(format NIL "I-~a" funcname) ',name)))
                    `(progn
                       (defgeneric ,,pkg-method (,',(gensym "MODULE") ,@',argsgeneric))
                       (defmacro ,,pkg-function (&whole ,',wholesymb ,@',args)
                         ,@',(when documentation (list documentation))
                         (declare (ignore ,@',(extract-macro-lambda-vars args)))
                         ,,(ecase type
                                  ((:macro 'macro)
                                   ``(apply #',,pkg-method ,,pkg-impl-var ,',wholesymb))
                                  ((:function 'function NIL)
                                   ```(apply #',',,pkg-method ,',,pkg-impl-var ,,',wholesymb)))))))))
        
        `(progn
           (defpackage ,fqpn
             (:nicknames ,(intern (string-upcase name) :KEYWORD))
             (:export ,@(append '(#:*implementation* #:implementation)
                                (mapcar #'(lambda (a) (make-symbol (string-upcase (car a)))) function-declarations))))
           (asdf:defsystem ,(intern (format nil "RADIANCE-~a" name))
             :class :interface  :interface-name ,(find-symbol (string-upcase name) :KEYWORD))
           (macrolet ((,macro-name ()
                        (let ((,pkg-impl-var (find-symbol "*IMPLEMENTATION*" ',name))
                              (,pkg-impl-fun (find-symbol "IMPLEMENTATION" ',name)))
                          `(progn
                             (defvar ,,pkg-impl-var NIL)
                             (declaim (inline ,,pkg-impl-fun))
                             (defun ,,pkg-impl-fun (&optional (,',new-impl-gens NIL ,',provided-gens))
                               (if ,',provided-gens
                                   (setf ,,pkg-impl-var ,',new-impl-gens)
                                   ,,pkg-impl-var))
                             ,,@(loop for declaration in function-declarations
                                   collect (destructuring-bind (funcname args &rest options) declaration
                                             (interface-function funcname args options)))))))
             (,macro-name))
           (find-package ',name))))))

(defmacro define-interface-function (function argslist &body body)
  (let ((pkg-method (find-symbol (format NIL "I-~a" function) (symbol-package function))))
    `(defmethod ,pkg-method ,argslist
       ,@body)))
