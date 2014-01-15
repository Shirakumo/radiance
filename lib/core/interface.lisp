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

;; Todo: Make extensible and configurable for component types.
(defmacro define-interface (name &body component-declarations)
  "Define a new implementation mechanism.
NAME                 ::= PRIMARY-NICKNAME | (PRIMARY-NICKNAME NICKNAME*)
COMPONENT-DECLARATION::= (NAME PRIMARY-ARGUMENTS OPTION*)
OPTION               ::= (OPTION-NAME VALUE*)

NAME                 --- A symbol used for the component's identifier.
PRIMARY-ARGUMENTS    --- A list of arguments used to build the component.
                         Depends on the component TYPE.
OPTION-NAME          --- A keyword identifying the supplied option.
PRIMARY-NICKNAME     --- NICKNAME used to build the fully qualified package name
                         in the form ofORG.TYMOONNEXT.RADIANCE.INTERFACE.name .
NICKNAME             --- A nickname symbol for the interface package.

OPTIONS:

 - :TYPE dictates the type of component being declared and may be one of
:FUNCTION :MACRO :CLASS. It defaults to :FUNCTION. If the TYPE is of MACRO or 
FUNCTION, the PRIMARY-ARGUMENTS list serves as the function/macro-lambda-list.
If the TYPE is of CLASS, it is used for the direct-slots of the defined class.

 - :DOCUMENTATION expects a string that is used as a documentation value for
the component, either as a function documentation string, or as the class 
documentation string.

 - * Any number of additional options may be passed along, but will not be
used if the TYPE is FUNCTION or MACRO. In the case of CLASS, the additional
options are passed directly to the class definition."
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
                      (argsgeneric args)
                      (call-function (if (or (find '&rest args)
                                             (find '&body args))
                                         'apply
                                         'funcall)))

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
                                   ``(,',call-function #',,pkg-method ,,pkg-impl-var (cdr ,',wholegens)))
                                  ((:function 'function NIL)
                                   ```(,',',call-function #',',,pkg-method ,',,pkg-impl-var ,@(cdr ,',wholegens))))))))))
        
        `(progn
           (defpackage ,fqpn
             (:nicknames ,@(mapcar #'(lambda (name) (intern (string-upcase name) :KEYWORD)) nicknames))
             (:export ,@(append '(#:*implementation* #:implementation)
                                (mapcar #'(lambda (a) (make-symbol (string-upcase (car a)))) component-declarations))))
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
                             ,,@(loop for declaration in component-declarations
                                   collect (destructuring-bind (specified-name args &rest options) declaration
                                             (ecase (second (assoc :type options))
                                               ((:macro :function 'macro 'function NIL)
                                                (interface-function specified-name args options))
                                               ((:class 'class)
                                                (interface-class specified-name args options)))))))))
             (,macro-name))
           (find-package ',name))))))

(defmacro define-interface-method (function argslist &body body)
  "Defines a new implementation of an interface function.

BODY       :== form*
ARGSLIST   :== specialized-lambda-list | ((:module IDENTIFIER [var]) specialized-lambda-list)
FUNCTION   --- A symbol
IDENTIFIER --- The module-identifier used for the implementation.

FUNCTION should be the symbol of an interface function. If the 
requested function cannot be be found in the package, a condition
of type NO-SUCH-INTERFACE-FUNCTION-ERROR will be signalled.

This macro will attempt to automatically retrieve the module-identifier
from the current package. If you want to set this manually or bind it
to a function-local variable, provide a first argument as a list,
formed in the way specified above. Note that this will be used as a
parameter-specializer-name, so it can be either a class-symbol or an
EQL form.

If you want to manually implement an interface function to get hold
of all of the CLOS functionality, you can always define your own
method on the respective INTERFACE::I-PUBLIC-FUNCTION-NAME generic."
  (let ((pkg-method (find-symbol (format NIL "I-~a" function) (symbol-package function))))
    (if pkg-method
        (progn
          (if (and (listp (first argslist)) (eq (caar argslist) :MODULE))
              (setf (car argslist) (list (or (caddar argslist) (gensym "MODULE"))
                                         (cadar argslist)))
              (push (list (gensym "MODULE") `(eql (module-identifier (get-module)))) argslist))
          (unless (find '&key argslist)
            (appendf argslist (list '&key)))
          `(defmethod ,pkg-method ,argslist
             ,@body))
        (error 'no-such-interface-function-error :interface (package-name (symbol-package function)) :interface-function function))))

(defclass interface (asdf:system)
  ((%interface-name :initarg :interface-name :initform NIL :accessor interface-name))
  (:documentation "Base class for radiance interfaces so modules may depend on them. 
Loading a system of this class will search for an interface definition in the
radiance configuration and attempt to load it. It is not guaranteed that the 
requested interface will be properly implemented after the load of this system."))

(defmethod asdf:operate ((op asdf:load-op) (interface interface) &key)
  (let* ((name (interface-name interface))
         (implementation (config-tree :implementation name)))
    (if implementation
        (let ((system (asdf:find-system implementation)))
          (asdf:load-system system)
          (let ((package (find-package name)))
            (if package
                (let* ((module-spec (assoc name (implementation-map system)))
                       (module (if (consp (cdr module-spec)) (second module-spec) (cdr module-spec))))
                  (if module
                      (setf (symbol-value (find-symbol "*IMPLEMENTATION*" package))
                            (etypecase module
                              (function (funcall module))
                              (symbol module)
                              (standard-object module)))))
                (error 'no-such-interface-error :interface name))))
        (error 'no-interface-implementation-error :interface name))))
