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

(defun macro-lambda-list->generic-list (macro-lambda-list)
  (loop with in-required-args = T
     for arg in macro-lambda-list
     collect (cond
               ((eql arg '&body)
                (setf in-required-args NIL)
                '&rest)
               ((or (eql arg '&rest) (eql arg '&key) (eql arg '&optional))
                (setf in-required-args NIL)
                arg)
               ((and in-required-args (listp arg))
                (gensym (format NIL "~{~a~^-~}" (extract-lambda-vars arg))))
               ((listp arg)
                (car arg))
               (T arg))))

(defun intern-list-symbols (list package)
  (loop for element in list
     collect (if (and (symbolp element) (not (keywordp element)))
                 (intern (string-upcase element) package)
                 element)))

(defmacro define-interface (name &body function-declarations)
  "Define a new implementation mechanism."
  (etypecase name
    (symbol)
    (list
     (assert (not (null name)) () "Interface name cannot be NIL.")
     (mapc #'(lambda (name) (assert (symbolp name) () "Interface names have to be symbols.")) name)))
  (with-gensyms
      ((macro-name    "INTERFACE-GENERATOR")
       (pkg-impl-var  "PKG-IMPL-VAR") (pkg-impl-fun  "PKG-IMPL-FUN")
       (pkg-function  "PKG-FUNCTION") (pkg-method    "PKG-METHOD")
       (pkg-class     "PKG-CLASS")
       (new-impl-gens "NEW-IMPL") (provided-gens "PROVIDED"))
    (let* ((nicknames (if (listp name) name (list name)))
           (name (if (listp name) (car name) name))
           (fqpn (intern (format NIL "ORG.TYMOONNEXT.RADIANCE.INTERFACE.~a" name) :KEYWORD)))
      (flet ((interface-class (classname slots options)
               (let ((slotsgens (gensym "SLOTS"))
                     (tmpgens (gensym "TEMP")))
                 `(let ((,pkg-class (find-symbol ,(format NIL "~a" classname) ',name))
                        (,slotsgens (loop for ,tmpgens in ',slots
                                       collect (intern-list-symbols ,tmpgens ',name))))
                    `(defclass ,,pkg-class () ,,slotsgens ,',@(remove :type options :key #'car)))))

             (interface-function (funcname args options)
               (let* ((documentation (second (assoc :documentation options)))
                      (type (second (assoc :type options)))
                      (wholegens (gensym "WHOLE"))
                      (args (make-key-extensible args))
                      (argsgeneric args))

                 ;; Fix up generic args for macro-lambda-lists.
                 (if (or (eql type :macro) (eql type 'macro))
                     (setf argsgeneric (macro-lambda-list->generic-list argsgeneric))
                     (setf argsgeneric (flatten-lambda-list argsgeneric)))
                 
                 ;; Triply nested macros. Woeyy.
                 `(let ((,pkg-function (find-symbol ,(format NIL "~a" funcname) ',name))
                        (,pkg-method (intern ,(format NIL "I-~a" funcname) ',name)))
                    `(progn
                       (defgeneric ,,pkg-method (,',(gensym "MODULE") ,@',argsgeneric))
                       (defmacro ,,pkg-function (&whole ,',wholegens ,@',args)
                         ,@',(when documentation (list documentation))
                         (declare (ignore ,@',(extract-macro-lambda-vars args)))
                         ,,(ecase type
                                  ((:macro 'macro)
                                   ``(apply #',,pkg-method ,,pkg-impl-var (cdr ,',wholegens)))
                                  ((:function 'function NIL)
                                   ```(apply #',',,pkg-method ,',,pkg-impl-var ,@(cdr ,',wholegens))))))))))
        
        `(progn
           (defpackage ,fqpn
             (:nicknames ,@(mapcar #'(lambda (name) (intern (string-upcase name) :KEYWORD)) nicknames))
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
                                   collect (destructuring-bind (specified-name args &rest options) declaration
                                             (ecase (second (assoc :type options))
                                               ((:macro :function 'macro 'function NIL)
                                                (interface-function specified-name args options))
                                               ((:class 'class)
                                                (interface-class specified-name args options)))))))))
             (,macro-name))
           (find-package ',name))))))

(defmacro define-interface-function (function argslist &body body)
  (let ((pkg-method (find-symbol (format NIL "I-~a" function) (symbol-package function))))
    (if pkg-method
        `(defmethod ,pkg-method ,argslist
           ,@body)
        (error 'no-such-interface-function-error :interface (package-name (symbol-package function)) :interface-function function))))

(defclass interface (asdf:system)
  ((%interface-name :initarg :interface-name :initform NIL :accessor interface-name)))

(defmethod asdf:operate ((op asdf:load-op) (interface interface) &key)
  (let* ((name (interface-name interface))
         (implementation (config-tree :implementation name)))
    (if implementation
        (let ((system (asdf:find-system implementation)))
          (asdf:load-system system)
          (let ((package (find-package name)))
            (if package
                (let* ((module-spec (assoc name (implement system)))
                       (module (if (consp (cdr module-spec)) (second module-spec) (cdr module-spec))))
                  (if module
                      (setf (symbol-value (find-symbol "*IMPLEMENTATION*" package))
                            (etypecase module
                              (function (funcall module))
                              (symbol module)
                              (standard-object module)))))
                (error 'no-such-interface-error :interface name))))
        (error 'no-interface-implementation-error :interface name))))
