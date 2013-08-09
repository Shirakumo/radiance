#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-flash-dispatch)

(implement 'dispatcher (get-module 'flash-dispatch))

(defmethod dispatch ((dispatch flash-dispatch) (request radiance:request) &key)
  (declare (optimize (speed 3)))
  (or
   (let ((hook (effective-trigger dispatch request)))
     (if hook (funcall (hook-function hook) (module hook))))
   (trigger :server :dispatch-default :args (list request))
   (dispatch-default dispatch request)))

(defmethod effective-trigger ((dispatch flash-dispatch) (request uri) &key)
  "Returns the trigger that would be called and the URI it registered that matches to it."
  (declare (optimize (speed 3)))
  (loop for (hook module uri) in (hooks dispatch)
     if (uri-matches uri request)
     do (return (loop for hook in (get-hooks :page hook)
                   for modsymb symbol = (module-symbol (module hook))
                   if (eql module modsymb)
                   return hook))))

(defmethod register ((dispatch flash-dispatch) (hook symbol) (module symbol) (uri uri) &key)
  (let ((found (find uri (hooks dispatch) :key #'third :test #'uri-same)))
    (when found
      (if (not (eql hook (first found)))
          (log:warn "Overriding existing trigger ~a on ~a" (car found) uri))
      (setf (hooks dispatch) (remove uri (hooks dispatch) :key #'third :test #'uri-same))))
  (log:info "Registering ~a for ~a/~a" uri module hook)
  (setf (hooks dispatch)
        (sort (append (hooks dispatch) `((,hook ,module ,uri)))
              #'sort-dispatcher-hooks))
  hook)

(defmethod dispatch-default ((dispatch flash-dispatch) (request radiance:request) &key &allow-other-keys)
  (error-page 404))

(defun sort-dispatcher-hooks (a b)
  (flet ((path (hook) (path (third hook))))
    (or (> (count #\/ (path a))
           (count #\/ (path b)))
        (> (length (path a))
           (length (path b))))))
