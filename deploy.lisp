#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(deploy:define-hook (:load load-modules) ()
  (deploy:status 0 "Configuring environment")
  (setf *deploying-p* T)
  ;; Set desired environment or quietly load separate default.
  (setf (environment) (if (boundp 'cl-user::environment)
                          (symbol-value 'cl-user::environment)
                          "deploy"))
  (deploy:status 1 "Loading target modules")
  ;; Load all target modules.
  (when (boundp 'cl-user::modules)
    (dolist (module (symbol-value 'cl-user::modules))
      (asdf:load-system module)))
  (deploy:status 1 "Performing boot to load required interfaces")
  (radiance:startup)
  (radiance:shutdown)
  ;; Clear ASDF/QL
  (mapc #'asdf:register-immutable-system (asdf:already-loaded-systems))
  (asdf:clear-source-registry)
  (setf asdf:*central-registry* NIL)
  #+quicklisp (setf ql:*local-project-directories* ()))

(deploy:define-hook (:deploy copy-resources) (directory)
  ;; Ensure expected directory structure:
  ;; bin/
  ;;   modules/
  ;;   config/
  ;;   data/
  ;;   cache/
  ;;   override/
  ;;     template/
  ;;     static/
  ;;   radiance.conf.lisp
  ;;   setup.lisp
  ;;   radiance
  (labels ((path (path)
             (merge-pathnames path directory)))
    (deploy:status 1 "Ensuring directory structure")
    (dolist (path '("modules/"
                    "config/"
                    "data/"
                    "cache/asdf/"
                    "override/template/"
                    "override/static/"))
      (ensure-directories-exist (path path)))
    (dolist (part '((:configuration "config/")
                    (:cache "cache/")
                    (:data "data/")
                    (:template "override/template/")
                    (:static "override/static/")))
      (deploy:copy-directory-tree
       (environment-directory T (first part))
       (path (second part)) :copy-root NIL))
    ;; Copy config file to root
    (uiop:copy-file (mconfig-pathname #.*package*)
                    (path "radiance.conf.lisp"))
    ;; Create setup file if necessary
    (with-open-file (stream (path "setup.lisp") :direction :output
                                                :if-exists NIL)
      (when stream
        (format stream ";;;; Radiance startup file. Loaded on init.~%(in-package :rad-user)~%")))
    ;; Copy module data
    (deploy:status 1 "Copying module data")
    (dolist (module (list-modules))
      (when (and (not (interface-p module)) (resolve-base module))
        (deploy:copy-directory-tree
         (merge-pathnames "template/" (resolve-base module))
         (path (make-pathname :directory `(:relative "modules" ,(module-name module) "template")))
         :copy-root NIL)
        (deploy:copy-directory-tree
         (merge-pathnames "static/" (resolve-base module))
         (path (make-pathname :directory `(:relative "modules" ,(module-name module) "static")))
         :copy-root NIL)))))

(deploy:define-hook (:boot setup-environment) ()
  (flet ((path (path)
           (merge-pathnames path (deploy:runtime-directory))))
    (deploy:status 0 "Configuring Radiance environment")
    (defmethod environment-directory (_ (kind (eql :configuration)))
      (path "config/"))

    (defmethod environment-directory (_ (kind (eql :cache)))
      (path "cache/"))

    (defmethod environment-directory (_ (kind (eql :data)))
      (path "data/"))

    (defmethod environment-directory (_ (kind (eql :template)))
      (path "override/template/"))

    (defmethod environment-directory (_ (kind (eql :static)))
      (path "override/static/"))

    (defun resolve-base (thing)
      (etypecase thing
        (pathname thing)
        (null (resolve-base *package*))
        ((or string symbol package)
         (path (make-pathname :directory `(:relative "modules" ,(module-name (modularize:module thing))))))))
    
    (defmethod ubiquitous:designator-pathname ((designator (eql #.*package*)) type)
      (make-pathname :name "radiance" :type (format NIL "conf.~(~a~)" type)
                     :defaults (deploy:runtime-directory)))
    (deploy:status 1 "Reloading config file")
    (reload-environment)
    (deploy:status 1 "Loading setup file")
    (setf asdf:*central-registry* (list (path "modules/")))
    (setf asdf:*user-cache* (path "cache/asdf/"))
    (when (probe-file (path "setup.lisp"))
      (load (path "setup.lisp")))))

(defun primitive-repl (&key (out *standard-output*) (in *standard-input*))
  (terpri out)
  (loop (format out "~%~a> " (package-name *package*))
        (finish-output out)
        (setf - (read in))
        (case -
          (:quit
           (radiance:shutdown)
           (uiop:quit))
          (:i
           (setf *package* (find-package (string (read)))))
          (T
           (shiftf /// // / (multiple-value-list (eval -)))
           (shiftf +++ ++ + -)
           (shiftf *** ** * (first /))
           (print *)))))

(defun startup-binary ()
  (radiance:startup)
  (deploy:status 0 "Server started up at http://~a~@[:~a~]"
                 (first (config :domains)) (config :port))
  ;; We are done starting up, now deregister systems so we can load/update later. Maybe.
  (asdf/system-registry:clear-registered-systems)
  ;; Primitive repl
  (setf *package* (find-package '#:rad-user))
  (if (open-stream-p *standard-input*)
      (primitive-repl)
      (loop while (started-p)
            do (sleep 60))))
