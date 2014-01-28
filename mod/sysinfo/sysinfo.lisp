#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-sysinfo)

(defpage site-info #u"inf./" (:lquery (template "sysinfo/index.html"))  
  ;Simple fields
  ($ "#acceptors" (text (concatenate-strings (config :ports) ", ")))
  ($ "#cur-requests" (text (format nil "~a" *radiance-request-count*)))
  ($ "#tot-requests" (text (format nil "~a" *radiance-request-total*)))

  ($ "#machine-instance" (text (machine-instance)))
  ($ "#machine-type" (text (machine-type)))
  ($ "#machine-version" (text (machine-version)))
  ($ "#software-type" (text (software-type)))
  ($ "#software-version" (text (software-version)))

  ;Modules
  (let ((nodes (loop for template = ($ "#module")
                  for module in *radiance-modules*
                  collect (create-module-node module ($ template (clone))))))
    ($ "#modules ul" (empty) (append nodes))))

(defun create-module-node (module template)
  (let ((module (asdf:find-system module)))
    ($ template "h3" (text (asdf:component-name module)))
    ($ template "blockquote" (text (asdf:system-description module)))
    (first template)))
