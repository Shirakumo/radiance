#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(implement 'core (get-module 'radiance-core))

(defmethod discover-modules ((core radiance-core) &key redefine reinitialize &allow-other-keys)
  (cl-fad:walk-directory (merge-pathnames "mod/" (pathname (config :root)))
                         (lambda (file) (load-module core file :redefine redefine :reinitialize reinitialize))
                         :test (lambda (file) 
                                 (let ((name (file-namestring file)))
                                   (equal ".mod" (subseq name (- (length name) 4)))))))

(defmethod load-module ((core radiance-core) file &key redefine reinitialize &allow-other-keys)
  (log:info "Discovered mod file: ~a (~a)" file (file-namestring file))
  (handler-bind ((module-already-initialized
                  #'(lambda (c) (declare (ignore c)) 
                            (cond
                              ((not (or redefine reinitialize)) (invoke-restart 'do-nothing))
                              ((and redefine reinitialize) (invoke-restart 'override-both))
                              (redefine (invoke-restart 'override-module))
                              (reinitialize (invoke-restart 'override-instance))))))
    (load file)))

(defmethod compile-module ((core radiance-core) (module module) &key force &allow-other-keys)
  (when (or (not (slot-value module 'compiled)) force)
    (log:info "Compiling module ~a (~a)" module (asdf-system module))
    (loop for dependency in (slot-value module 'dependencies)
       do (compile-dependency core dependency :force force))
    (ql:quickload (asdf-system module))
    (setf (slot-value module 'compiled) T)))

(defmethod compile-module ((core radiance-core) (module string) &key force &allow-other-keys)
  (let ((module-instance (get-module module)))
    (if module-instance
        (compile-module core module-instance :force force)
        (error "Module ~a not found." module))))

(defmethod compile-module ((core radiance-core) (module symbol) &key force &allow-other-keys)
  (compile-module core (symbol-name module) :force force))

(defmethod compile-dependency ((core radiance-core) (dependency symbol) &key force &allow-other-keys)
  (cond
    ((get-module dependency) (compile-module core dependency :force force))
    ((config-tree :implementations (make-keyword dependency)) (compile-module core (config-tree :implementations (make-keyword dependency)) :force force))
    (T (error "Dependency ~a not found!" dependency))))

(defmethod load-implementations ((core radiance-core) &key force &allow-other-keys)
  (loop for (key . val) in (config :implementations)
     do (progn (log:info "Choosing ~a for ~a." val key)
               (if (listp val)
                   (loop for val in val 
                      do (compile-module core val :force force))
                   (compile-module core val :force force)))))
