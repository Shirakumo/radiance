(in-package #:org.shirakumo.radiance.core)

(define-version-migration radiance-core (NIL 2.0.0)
  (let ((previous-config-directory (merge-pathnames "radiance/" (ubiquitous:config-directory))))
    (when (uiop:directory-exists-p previous-config-directory)
      (l:info :radiance.migration "Migrating previous configuration from ~a"
              previous-config-directory)
      (loop for original-path in (uiop:subdirectories previous-config-directory)
            for environment = (car (last (pathname-directory original-path)))
            for new-path = (environment-directory environment :configuration)
            for temp-path = (make-pathname :directory (append (butlast (pathname-directory new-path))
                                                              (list (format NIL "before-migration-~a" environment)))
                                           :defaults new-path)
            do (when (uiop:directory-exists-p new-path)
                 (rename-file new-path temp-path)
                 (l:info :radiance.migration "The existing configuration for the ~s environment has been moved to~%  ~a"
                         environment temp-path))
               (rename-file original-path new-path)
               (when (string= environment (environment))
                 (l:info :radiance.migration "Restored current environment ~s from old site, reloading."
                         environment)
                 (reload-environment)))
      (uiop:delete-empty-directory previous-config-directory))))
