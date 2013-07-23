#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(implement 'dispatcher (get-module 'flash-dispatch))

(defmethod dispatch ((dispatch flash-dispatch) (request radiance:request) &key)
  "Dispatches a request to a module based on a subdomain."
  (loop with response = NIL
     for (trigger subdomain domain port path) in (hooks dispatch)
     if (and (or (not subdomain) (string-equal subdomain (concatenate-strings (subdomains request) ".")))
             (or (not domain) (string-equal domain (domain request)))
             (or (not port) (= port (port request)))
             (or (not path) (and (<= (length path) (length (path request)))
                                 (string= path (path request) :end2 (length path)))))
     do (setf response (trigger trigger))
     finally (return (or response (read-data-file "static/html/hello.html")))))

(defmethod register ((dispatch flash-dispatch) trigger &key subdomain domain port path)
  "Registers a subdomain for a module. If the subdomain is NIL, all unhandled requests are dispatched to the module."
  (setf (hooks dispatch) 
        (sort (append (hooks dispatch) `((,trigger ,subdomain ,domain ,port ,path)))
              #'sort-dispatcher-hooks))
  trigger)

(defun count-path (path)
  (if (or (eq path NIL) (equal path "/")) 0
      (length (split-sequence:split-sequence #\/ (string-left-trim "/" path)))))

(defun sort-dispatcher-hooks (a b)
  (flet ((path (hook) (car (last hook))))
    (> (count-path (path a))
       (count-path (path b)))))
