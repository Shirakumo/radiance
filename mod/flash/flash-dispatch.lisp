#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-flash-dispatch)

(defvar *hooks* ())

(define-interface-method dispatcher:dispatch (request)
  (or
   (let ((hook (dispatcher:effective-trigger request)))
     (when hook
       (v:debug :flash-dispatch "Dispatching to hook: ~a" hook)
       (funcall (item-function hook))))
   (trigger :server :dispatch-default)
   (dispatcher:dispatch-default request)))

(define-interface-method dispatcher:effective-trigger (request)
  (loop for (hook identifier uri) in *hooks*
        if (uri-matches uri request)
          do (return (loop for item in (hook-items :page hook)
                           if (eql identifier (item-identifier item))
                             return item))))

(define-interface-method dispatcher:register (hook identifier uri)
  (let ((found (find uri *hooks* :key #'third :test #'uri-same)))
    (when found
      (if (not (eql hook (first found)))
          (v:warn :flash-dispatch "Overriding existing trigger ~a on ~a" (car found) uri))
      (setf *hooks* (delete uri *hooks* :key #'third :test #'uri-same))))
  (v:debug :flash-dispatch "Registering ~a for ~a/~a" uri identifier hook)
  (setf *hooks* (sort (append *hooks* `((,hook ,identifier ,uri))) #'sort-dispatcher-hooks))
  hook)

(define-interface-method dispatcher:unregister (uri)
  )

(define-interface-method dispatcher:dispatch-default (request)
  (error-page 404))

(defun sort-dispatcher-hooks (a b)
  (flet ((path (hook) (path (third hook))))
    (or (> (count #\/ (path a))
           (count #\/ (path b)))
        (> (length (path a))
           (length (path b))))))
