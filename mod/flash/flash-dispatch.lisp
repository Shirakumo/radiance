#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-flash-dispatch)

(implement 'dispatcher (get-module 'flash-dispatch))

(defmethod dispatch ((dispatch flash-dispatch) (request radiance:request) &key)
  (or 
   (loop for (trigger . uri) in (hooks dispatch)
      if (uri-matches uri request)
      do (return (trigger :page trigger)))
   (trigger :server :dispatch-default request)
   (dispatch-default dispatch request)))

(defmethod register ((dispatch flash-dispatch) trigger uri &key)
  (let ((found (find uri (hooks dispatch) :key #'cdr :test #'uri-same)))
    (when found
      (if (not (eql trigger (car found)))
          (log:warn "Overriding existing trigger ~a on ~a" (car found) uri))
      (setf (hooks dispatch) (remove uri (hooks dispatch) :key #'cdr :test #'uri-same))))
  
  (setf (hooks dispatch)
        (sort (append (hooks dispatch) `((,trigger . ,uri)))
              #'sort-dispatcher-hooks))
  trigger)

(defmethod dispatch-default ((dispatch flash-dispatch) (request radiance:request) &key &allow-other-keys)
  (read-data-file "static/html/hello.html"))

(defun sort-dispatcher-hooks (a b)
  (flet ((path (hook) (path (cdr hook))))
    (or (> (count #\/ (path a))
           (count #\/ (path b)))
        (> (length (path a))
           (length (path b))))))
