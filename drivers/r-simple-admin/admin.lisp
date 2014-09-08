#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-admin
  (:use #:cl #:radiance))
(in-package #:simple-admin)

(defvar *categories* (make-hash-table :test 'equalp))
(defvar *prepared-categories* NIL)

(defun prepare-categories ()
  (setf *prepared-categories*
        (loop for title being the hash-keys of *categories*
              for panels being the hash-values of *categories*
              collect (clip:make-clipboard
                       :title title
                       :panels (loop for panel being the hash-values of panels
                                     collect panel)))))

(defun admin::category (category)
  (gethash (string category) *categories*))

(defun admin::add-category (category)
  (setf (gethash (string category) *categories*)
        (make-hash-table :test 'equalp)))

(defun admin::remove-category (category)
  (remhash (string category) *categories*))

(defun admin:panel (category name)
  (let ((category (admin::category category)))
    (when category
      (gethash (string name) category))))

(defun (setf admin:panel) (function category name)
  (setf (gethash (string name)
                 (or (admin::category category)
                     (admin::add-category category)))
        function)
  (prepare-categories))

(defun admin:remove-panel (category name)
  (when (admin:panel category name)
    (remhash (string name) (admin::category category))
    (when (= 0 (hash-table-count (admin::category category)))
      (admin::remove-category category))))

;; Fix the dumb options thing some how, idfk
(defmacro admin:define-panel (name category options &body body)
  (let ((name (string-downcase name))
        (category (string-downcase category)))
    (destructuring-bind (&key icon &allow-other-keys) options
      `(setf (admin:panel ,category ,name)
             (clip:make-clipboard
              :title ,(string name)
              :url ,(format NIL "/~a/~a" category name)
              :icon ,icon
              :function
              #'(lambda ()
                  ,@body))))))

(defun run-panel (category panel)
  (let ((panel (admin:panel category panel)))
    (when panel
      (let ((result (funcall (clip:clip panel :function))))
        (etypecase result
          (string result)
          (plump:node (with-output-to-string (s)
                        (plump:serialize result s)))
          (array (lquery:$ result (serialize) (node))))))))

(define-page admin-index #@"admin/([^/]*)(/(.+))?" (:uri-groups (category NIL panel) :lquery (template "index.ctml"))
  (let ((manage (post/get "simple-admin-manage"))
        (action (post-var "simple-admin-action")))
    (r-clip:process
     (lquery:$ (node))
     :manage manage
     :categories *prepared-categories*
     :content (or (when manage
                    (cond ((not action)
                           (plump:parse (template "confirm.ctml")))
                          ((not (string-equal action "yes"))
                           NIL)
                          ((string= manage "shutdown")
                           (bt:make-thread #'(lambda () (sleep 1) (radiance:shutdown)))
                           "Shutting down.")
                          ((string= manage "restart")
                           (bt:make-thread #'(lambda () (sleep 1) (radiance:shutdown) (radiance:startup)))
                           "Restarting.")))
                  (run-panel category panel)))))

(admin:define-panel index admin (:icon "fa-home")
  "Hi.")
