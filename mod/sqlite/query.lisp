#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sqlite)

(define-interface-method db:query (&rest forms)
  (multiple-value-bind (query data) (if (cdr forms)
                                        (%query :and forms)
                                        (%query (car (first forms)) (cdr (first forms))))
    `(cons ,query (alexandria:flatten (list ,@data)))))

(defgeneric %query (action args))

(defmethod %query ((action (eql :and)) args)
  (loop with datas = ()
     for arg in args
     for (part data) = (multiple-value-list (%query (car arg) (cdr arg)))
     collect part into query
     do (nappend datas data)
     finally (return (values (format NIL "(~{~a~^ AND ~})" query) datas))))

(defmethod %query ((action (eql :or)) args)
  (loop with datas = ()
     for arg in args
     for (part data) = (multiple-value-list (%query (car arg) (cdr arg)))
     collect part into query
     do (nappend datas data)
     finally (return (values (format NIL "(~{~a~^ OR ~})" query) datas))))

(defmethod %query ((action (eql :not)) args)
  (let ((res (if (cdr args)
                 (%query :and args)
                 (%query (car (first args)) (cdr (first args))))))
    (multiple-value-bind (query data) res
      (values (format NIL "(NOT ~a)" query) data))))

(defmethod %query ((action (eql :in)) args)
  (values (format NIL "`~a` IN (~{?~*~^, ~})" (first args) (second args)) (list (second args))))

(defmethod %query ((action (eql :matches)) args)
  (values (format NIL "`~a` REGEXP ?" (first args)) (list (second args))))

(defmacro def-ops ()
  `(progn ,@(loop for op in '(:= :>= :<= :> :<)
               collect `(defmethod %query ((action (eql ,op)) args)
                          (values (format NIL ,(format NIL "`~~a` ~a ?" op) (first args)) (list (second args)))))))
(def-ops)


