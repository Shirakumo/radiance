#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-kickstart)

(defmethod dispatch ((dispatch kickstart) (request radiance:request) &key)
  (let ((subdomains (concatenate-strings (subdomains request) "."))
        (domain (domain request))
        (port (port request))
        (path (path request)))
    (let ((data (model-get T "kickstart-starters"
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

(defmethod register ((dispatch kickstart) trigger uri &key)
  (setf trigger (if (symbolp trigger) (symbol-name trigger) (string-upcase trigger)))
  (let ((model (model-hull T "kickstart-starters")))
    (with-fields ((trig "trigger") subdomain domain port path) model
      (setf trig trigger
            subdomain (concatenate-strings (subdomains uri) ".")
            domain (domain uri)
            port (port uri)
            path (path uri))
      (model-insert model))))
