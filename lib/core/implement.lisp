#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defun flatten-lambda-list (lambda-list)
  (mapcar #'(lambda (a) (if (listp a) (car a) a)) lambda-list))

(defun extract-lambda-vars (lambda-list)
  (remove-if #'(lambda (a) (find a '(&allow-other-keys &aux &body &environment &key &optional &rest &whole)))
             (flatten-lambda-list lambda-list)))

(defmacro define-interface (name &body function-declarations)
  "Define a new implementation mechanism."
  (with-gensyms
      ((macro-name   "INTERFACE-GENERATOR")
       (pkg-impl-var "PKG-IMPL-VAR")
       (pkg-impl-fun "PKG-IMPL-FUN")
       (pkg-function "PKG-FUNCTION")
       (pkg-method   "PKG-METHOD")
       (pkg-module   "PKG-MODULE")
       (restsymb     "REST")
       (wholesymb    "WHOLE"))
    (let ((fqpn (intern (format NIL "ORG.TYMOONNEXT.RADIANCE.INTERFACE.~a" name) :KEYWORD)) )
      (flet ((interface-function (funcname args options)
               (let* ((documentation (second (assoc :documentation options)))
                      (type (second (assoc :type options)))
                      (argsvarlist)
                      (argsgeneric (cons pkg-module (copy-list args))))

                 ;; Add rest and allow-other-keys if missing.
                 (unless (or (find '&body args) (find '&rest args))
                   (let ((keypos (position '&key args)))
                     (if keypos
                         (progn
                           (setf argsgeneric (append argsgeneric '(&allow-other-keys)))
                           (if (> keypos 0)
                               (progn
                                 (push restsymb (cdr (nthcdr (1- keypos) args)))
                                 (push '&rest (cdr (nthcdr (1- keypos) args))))
                               (progn
                                 (push restsymb args)
                                 (push '&rest args))))
                         (progn
                           (setf argsgeneric (append argsgeneric '(&key &allow-other-keys)))
                           (append args (list '&rest restsymb))))))

                 (setf argsvarlist args)
                 ;; Macro lambda-list handling
                 (when (or (eql type :macro) (eql type 'macro))
                   (loop for i from 0 below (length argsgeneric)
                      for arg in argsgeneric
                      until (or (eql arg '&rest) (eql arg '&key) (eql arg '&optional))
                      do (when (listp arg)
                           (let ((gensym (gensym (format NIL "~{~a~^-~}" (extract-lambda-vars arg)))))
                             (alexandria:appendf argsvarlist arg)
                             (setf (nth i argsgeneric) gensym))))
                   (let ((bodypos (position '&body argsgeneric)))
                     (when bodypos (setf (nth bodypos argsgeneric) '&rest))))
                 
                 ;; Create pure var lists.
                 (setf argsvarlist (extract-lambda-vars argsvarlist))
                 (setf argsgeneric (flatten-lambda-list argsgeneric))
                 
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
           (asdf:defsystem ,(intern (format nil "RADIANCE-~a" name))
             :class :interface
             :interface-name ,(find-symbol (string-upcase name) :KEYWORD))
           (macrolet ((,macro-name ()
                        (let ((,pkg-impl-var (find-symbol "*IMPLEMENTATION*" ',name))
                              (,pkg-impl-fun (find-symbol "IMPLEMENTATION" ',name))
                              (new-impl-gens (gensym "NEW-IMPL"))
                              (provided-gens (gensym "PROVIDED")))
                          `(progn
                             (defvar ,,pkg-impl-var NIL)
                             (declaim (inline ,,pkg-impl-fun))
                             (defun ,,pkg-impl-fun (&optional (,new-impl-gens NIL ,provided-gens))
                               (if ,provided-gens
                                   (setf ,,pkg-impl-var ,new-impl-gens)
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
