#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defvar *deploying-p* NIL)
(defvar *environment* NIL)

(define-hook environment-change ())

(defgeneric environment-directory (environment type))
(defgeneric environment-module-directory (module type))

(defmethod environment-directory (environment kind)
  (ecase kind
    (:configuration
     (let ((base (or* (uiop:getenv "XDG_CONFIG_HOME")
                      #+windows (uiop:getenv "AppData")
                      #+windows (make-pathname :directory '(:absolute :home "Application Data"))
                      (make-pathname :directory '(:absolute :home ".config")))))
       (merge-pathnames (make-pathname :directory `(:relative "radiance" ,environment))
                        (etypecase base
                          (pathname base)
                          (string (uiop:parse-native-namestring base))))))
    (:cache
     (let ((base (or* (uiop:getenv "XDG_CACHE_HOME")
                      #+windows (uiop:getenv "TEMP")
                      #+windows (make-pathname :directory '(:absolute :home "Local Settings" "Temp"))
                      (make-pathname :directory '(:absolute :home ".cache")))))
       (merge-pathnames (make-pathname :directory `(:relative "radiance" ,environment))
                        (etypecase base
                          (pathname base)
                          (string (uiop:parse-native-namestring base))))))
    ((:data :template :static)
     (let ((base (or* (uiop:getenv "XDG_DATA_HOME")
                      #+windows (uiop:getenv "LocalAppData")
                      #+windows (make-pathname :directory '(:absolute :home "Local Settings"))
                      (make-pathname :directory '(:absolute :home ".local" "share")))))
       (merge-pathnames (make-pathname :directory `(:relative "radiance" ,environment ,(string-downcase kind)))
                        (etypecase base
                          (pathname base)
                          (string (uiop:parse-native-namestring base))))))))

(defmethod environment-directory ((environment (eql T)) kind)
  (environment-directory *environment* kind))

(defmethod environment-module-directory (module kind)
  (check-environment)
  (let ((name (module-name module)))
    (merge-pathnames
     (make-pathname :directory `(:relative ,(string-downcase name)))
     (environment-directory T kind))))

(defun environment-module-pathname (module kind pathname)
  (merge-pathnames
   pathname
   (environment-module-directory module kind)))

(defun environment ()
  *environment*)

(defun (setf environment) (environment)
  (check-type environment string)
  ;; Clear config
  (dolist (module (list-modules))
    (setf (module-storage module :config) NIL))
  ;; Update environment
  (setf *environment* environment)
  ;; Reload configuration
  (handler-case
      (ubiquitous:restore #.*package*)
    (ubiquitous:no-storage-file ()
      (warn "Configuration for ~s not found-- creating from defaults." environment)
      (ubiquitous:restore (asdf:system-relative-pathname :radiance "default-config.lisp"))
      (ubiquitous:offload #.*package*)))
  (trigger 'environment-change))

(defun check-environment ()
  (restart-case
      (unless *environment*
        (error 'environment-not-set))
    (continue ()
      :report "Use and load the default environment."
      (setf (environment) "default"))
    (set-environment (environment)
      :report "Set and load a specific environment."
      :interactive (lambda () (list (read *query-io*)))
      (setf (environment) environment))))

(defun reload-environment ()
  (check-environment)
  (setf (environment) (environment)))

(defun sync-environment ()
  (dolist (module (modularize:list-modules) T)
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (ubiquitous:offload))))

(defun mconfig-pathname (module &optional (type :lisp))
  (make-pathname :name (string-downcase (module-name module))
                 :type (format NIL "conf.~(~a~)" type)
                 :defaults (environment-module-directory module :configuration)))

(defmethod ubiquitous:designator-pathname ((designator package) type)
  (mconfig-pathname designator type))

(defun mconfig-storage (module)
  (let* ((module (module module))
         (*package* module))
    (or (module-storage module :config)
        (setf (module-storage module :config)
              (ubiquitous:with-local-storage (module :transaction NIL)
                (handler-bind ((ubiquitous:no-storage-file
                                 (lambda (err)
                                   (declare (ignore err))
                                   (invoke-restart 'ubiquitous:use-new-storage
                                                   (make-hash-table :test 'equal)))))
                  (ubiquitous:restore))
                (ubiquitous:offload))))))

(defun mconfig (module &rest path)
  (let ((module (module module)))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'ubiquitous:value path))))

(defun (setf mconfig) (value module &rest path)
  (let* ((module (module module))
         (*package* module))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'(setf ubiquitous:value) value path))))

(defun defaulted-mconfig (default module &rest path)
  (let* ((module (module module))
         (*package* module))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'ubiquitous:defaulted-value default path))))

(defun remmconfig (module &rest path)
  (let* ((module (module module))
         (*package* module))
    (ubiquitous:with-local-storage (module :storage (mconfig-storage module))
      (apply #'ubiquitous:remvalue path))))

(defmacro config (&rest path)
  `(mconfig ,*package* ,@path))

(defmacro defaulted-config (default &rest path)
  `(defaulted-mconfig ,default ,*package* ,@path))

(defmacro remconfig (&rest path)
  `(remmconfig ,*package* ,@path))

(defun static-file (namestring &optional base)
  (let ((base (or base *package*)))
    (or (probe-file (environment-module-pathname base :static namestring))
        (merge-pathnames namestring (merge-pathnames "static/" (merge-pathnames (resolve-base base)))))))

(defmacro @static (&environment env namestring)
  (if (and (constantp namestring env)
           (not *deploying-p*))
      `(load-time-value (static-file ,namestring ,*package*))
      `(static-file ,namestring ,*package*)))

(defun template-file (namestring &optional base)
  (let ((base (or base *package*)))
    (or (probe-file (environment-module-pathname base :template namestring))
        (merge-pathnames namestring (merge-pathnames "template/" (merge-pathnames (resolve-base base)))))))

(defmacro @template (&environment env namestring)
  (if (and (constantp namestring env)
           (not *deploying-p*))
      `(load-time-value (template-file ,namestring ,*package*))
      `(template-file ,namestring ,*package*)))
