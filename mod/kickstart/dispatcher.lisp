#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-kickstart)

(defhook 'kickstart-main-hook (get-module 'kickstart) #'dispatch)

(if (implementation 'dispatcher)
    (defhook :dispatch-default (get-module 'kickstart) #'dispatch)
    (implement 'dispatcher (get-module 'kickstart)))

(defmethod dispatch ((dispatch kickstart) (request radiance:request) &key)
  (let ((subdomains (concatenate-strings (subdomains request) "."))
        (domain (domain request))
        (port (port request))
        (path (path request)))
    (let ((data (model-get (implementation 'data-model) "kickstart-starters"
                           (:and (:or (:= "subdomain" subdomains)
                                      (:= "subdomain" ""))
                                 (:or (:= "domain" domain)
                                      (:= "domain" ""))
                                 (:or (:= "port" port)
                                      (:= "port" ""))))))
      (or 
       (loop for starter in data
          if (or (= (length path) 0)
                 (cl-ppcre:scan (model-field starter "path") path))
          do (return (trigger (make-keyword (model-field starter "trigger")))))
       (dispatch-default dispatch request)))))

(defmethod dispatch-default ((dispatch kickstart) (request radiance:request) &key)
  (read-data-file "static/html/error/404.html"))

(defmethod register ((dispatch kickstart) trigger &key (subdomain "") (domain "") (port "") (path ""))
  (setf trigger (if (symbolp trigger) (symbol-name trigger) (string-upcase trigger)))
  (let ((model (model-hull (implementation 'data-model) "kickstart-starters")))
    (setf (model-field model "trigger") trigger
          (model-field model "subdomain") subdomain
          (model-field model "domain") domain
          (model-field model "port") port
          (model-field model "path") path)
    (model-insert model)))

(defgeneric unregister (dispatcher trigger &key subdomain domain port path)
  (:documentation "Unregisters a matching trigger."))
(defmethod unregister ((dispatch kickstart) trigger &key subdomain domain port path)
  (setf trigger (if (symbolp trigger) (symbol-name trigger) (string-upcase trigger)))
  (db-remove (implementation 'database) "kickstart-starters"
             (apply #':and (append (list (:= "trigger" trigger))
                                   (if subdomain (list (:= "subdomain" subdomain)))
                                   (if domain (list (:= "domain" domain)))
                                   (if port (list (:= "port" port)))
                                   (if path (list (:= "path" path)))))))
