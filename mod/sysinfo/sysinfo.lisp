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
                  for module being the hash-values of *radiance-modules*
                  collect (create-module-node module ($ template (clone))))))
    ($ "#modules ul" (empty) (append nodes)))
  
  ;Implementations
  (let ((nodes (loop for impl being the hash-keys of *radiance-implements*
                    collect (create-implementation-node impl))))
    ($ "#implementations" (append nodes))))

(defun create-module-node (module template)
  ($ template "h3" (text (slot-value module 'radiance::name)))
  ($ template "blockquote" (text (documentation (class-of module) 'type)))
  (let ((nodes (loop for slotdef in (closer-mop:class-slots (class-of module))
                  when (create-slot-node slotdef)
                  collect it)))
    ($ template ".modinfo tbody" (append nodes)))
  (first template))

(defun create-slot-node (slotdef)
  (let ((location (closer-mop:slot-definition-location slotdef)))
    (when (consp location)
      (first (lquery:parse-html
              (format nil "<tr><td>~a</td><td>~a</td></tr>" (car location)
                      (hunchentoot:escape-for-html (format nil "~a" (cdr location)))))))))

(defun create-implementation-node (impl)
  (first (lquery:parse-html
          (format nil "<div class=\"key-val\"><label>~a</label><span>~a</span></div>" impl 
                  (hunchentoot:escape-for-html (format nil "~a" (implementation impl)))))))
