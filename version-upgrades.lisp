#|
 This file is a part of Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(define-version-migration radiance-core (NIL 2.0.0)
  (let ((previous-config-directory (merge-pathnames "radiance/" (ubiquitous:config-directory))))
    (when (uiop:directory-exists-p previous-config-directory)
      (l:info :radiance.migration "Migrating previous configuration from ~a."
              previous-config-directory)
      (loop for original-path in (uiop:subdirectories previous-config-directory)
            for environment = (car (last (pathname-directory original-path)))
            for new-path = (environment-directory environment :configuration)
            do (when (uiop:directory-exists-p new-path)
                 (rename-file new-path
                              (make-pathname :directory (append (butlast (pathname-directory new-path))
                                                                (list (format NIL "before-migration-~a" environment)))
                                             :defaults new-path)))
               (rename-file original-path new-path))
      (uiop:delete-empty-directory previous-config-directory))))
