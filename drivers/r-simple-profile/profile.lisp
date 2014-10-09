#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:simple-profile
  (:use #:cl #:radiance)
  (:implements #:profile))
(in-package #:simple-profile)

(define-trigger db:connected ()
  (db:create 'simple-profile-fields '((:name (:varchar 32)) (:type (:varchar 16)) (:default (:varchar 128)) (:editable (:integer 1)))
             :indices '(:name)))

(defun normalize (user)
  (etypecase user
    (user:user user)
    (null (user:get :anonymous))
    ((or string symbol) (user:get user :if-does-not-exist :error))))

(defun profile:avatar (user size)
  (let ((email (or (user:field (normalize user) "email") "")))
    (format NIL "//www.gravatar.com/avatar/~a?s=~d&d=blank"
            (cryptos:md5 (string-downcase email)) size)))

(defun profile:name (user)
  (let ((user (normalize user)))
    (or* (user:field user "displayname")
         (user:username user))))

(defun profile:page (user &optional (panel :profile))
  (declare (ignore user page-type))
  "")

(defun profile:fields ()
  (dm:get 'simple-profile-fields (db:query :all)))

(defun profile:add-field (name &key (type :text) default (editable T))
  (let ((name (string-downcase name)))
    (unless (db:select 'simple-profile-fields (db:query (:= 'name name)))
      (let ((type (string-downcase type)))
        (assert (member type '(text textarea password email url time date datetime datetime-local month week color number range checkbox radio file tel) :test #'string-equal)
                () "TYPE must be one of (text textarea password email url time date datetime datetime-local month week color number range checkbox radio file tel).")
        (db:insert 'simple-profile-fields `((name . ,name) (type . ,type) (default . ,(or default "")) (editable . ,(if editable 1 0)))))
      name)))

(defvar *panels* (make-hash-table :test 'equalp))
(defvar *cached-panels* ())

(defun generate-panel-cache ()
  (setf *cached-panels*
        (sort (loop for panel being the hash-values of *panels*
                    collect panel)
              #'string> :key #'(lambda (a) (clip:clip a :name)))))

(defun profile:panel (name)
  (gethash (string name) *panels*))

(defun (setf profile:panel) (function name)
  (setf (gethash (string name) *panels*)
        function)
  (generate-panel-cache))

(defun profile:remove-panel (name)
  (remhash (string name) *panels*)
  (generate-panel-cache))

(defvar *panel-options* (make-hash-table))

(defun (setf panel-option) (function option)
  (setf (gethash option *panel-options*) function))

(define-options-definer profile:define-panel-option panel-option (namevar bodyvar valuevar))

(defmacro profile:define-panel (name options &body body)
  (let ((name (string-downcase name)))
    (destructuring-bind (&key access (user (gensym "USER")) &allow-other-keys) options
      (multiple-value-bind (body forms) (expand-options *panel-options* options body name)
        (declare (ignore forms))
        `(setf (profile:panel ,name)
               (clip:make-clipboard
                :name ,name
                :access ',access
                :function
                #'(lambda (,user)
                    (declare (ignorable ,user))
                    ,@body)))))))

(defun run-panel (panel user)
  (let ((panel (profile:panel panel)))
    (when panel
      (let ((result (funcall (clip:clip panel :function) user)))
        (etypecase result
          (null "")
          (string result)
          (plump:node (with-output-to-string (s)
                        (plump:serialize result s)))
          (array (lquery:$ result (serialize) (node))))))))

(define-page user #@"user/([^/]+)?(/([^/]+))?" (:uri-groups (username NIL panel) :lquery (template "public.ctml"))
  (let ((user (user:get username)))
    (if user
        (r-clip:process
         T
         :user user
         :you (auth:current)
         :panels *cached-panels*
         :panel-name (or* panel "index")
         :panel (run-panel (or* panel "index") user))
        (error 'request-not-found :message "No such user."))))
