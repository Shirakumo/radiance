#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(implement 'dispatcher (get-module 'flash-dispatch))

(defmethod dispatch ((dispatch flash-dispatch) (request radiance:request) &key)
  "Dispatches a request to a module based on a subdomain."
  (handler-case (authenticate (implementation 'auth))
    (auth-error (err) (log:warn "~a" err)))
  (loop for trigger being the hash-keys of (table dispatch)
     for specific being the hash-values of (table dispatch)
     collect (destructuring-bind (subdomain domain port path) specific
               (when (and (or (not subdomain) (string-equal subdomain (concatenate-strings (subdomains request) ".")))
                          (or (not domain) (string-equal domain (domain request)))
                          (or (not port) (= port (port request)))
                          (or (not path) (and (<= (length path) (length (path request)))
                              (string= path (path request) :end2 (length path)))))
                 (trigger trigger)))
     into responses
     finally (progn (setf responses (alexandria:flatten responses))
                    (return (if responses
                               responses
                               (read-data-file "static/html/hello.html"))))))

(defmethod register ((dispatch flash-dispatch) trigger &key subdomain domain port path)
  "Registers a subdomain for a module. If the subdomain is NIL, all unhandled requests are dispatched to the module."
  (setf (gethash trigger (table dispatch)) (list subdomain domain port path))
  trigger)
