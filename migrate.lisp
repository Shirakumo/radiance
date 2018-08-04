#|
 This file is a part of Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defun encode-version (version)
  (etypecase version
    (keyword version)
    (string (intern version :KEYWORD))
    (cons (encode-version
           (with-output-to-string (out)
             (princ (first version) out)
             (dolist (part (rest version))
               (etypecase part
                 (integer (format out ".~d" part))
                 (string (format out "-~:@(~a~)" part)))))))))

(defun parse-version (version)
  (loop for part in (cl-ppcre:split "[.-]" version)
        collect (or (ignore-errors (parse-integer part))
                    part)))

(defun ensure-parsed-version (version)
  (etypecase version
    (cons version)
    ((or string keyword)
     (parse-version (string version)))))

(defun ensure-versions-comparable (a b)
  (let* ((a (ensure-parsed-version a))
         (b (ensure-parsed-version b))
         (al (length a))
         (bl (length b)))
    (cond ((< al bl)
           (values (append a (make-list (- bl al) :initial-element 0))
                   b))
          ((< bl al)
           (values a
                   (append b (make-list (- al bl) :initial-element 0))))
          (T
           (values a b)))))

(defmethod version-part= ((a integer) (b integer)) (= a b))
(defmethod version-part= ((a integer) (b string))  NIL)
(defmethod version-part= ((a string)  (b integer)) NIL)
(defmethod version-part= ((a string)  (b string))  (string= a b))
(defmethod version-part< ((a integer) (b integer)) (< a b))
(defmethod version-part< ((a integer) (b string))  T)
(defmethod version-part< ((a string)  (b integer)) NIL)
(defmethod version-part< ((a string)  (b string))  (string< a b))

(defun version= (a b)
  (multiple-value-bind (a-parts b-parts) (ensure-versions-comparable a b)
    (loop for a in a-parts
          for b in b-parts
          always (version-part= a b))))

(defun version< (a b)
  (multiple-value-bind (a-parts b-parts) (ensure-versions-comparable a b)
    (loop for a in a-parts
          for b in b-parts
          do (cond ((version-part= a b))
                   ((version-part< a b) (return T))
                   (T                   (return NIL)))
          finally (return NIL))))

(defun version<= (a b)
  (multiple-value-bind (a-parts b-parts) (ensure-versions-comparable a b)
    (loop for a in a-parts
          for b in b-parts
          do (cond ((version-part= a b))
                   ((version-part< a b) (return T))
                   (T                   (return NIL)))
          finally (return T))))

(defun version-region (versions &key start end)
  (loop for version in versions
        when (and (or (not start) (version<= start version))
                  (or (not end) (version<= version end)))
        collect version))

(defun version-bounds (versions &key start end)
  (let* ((versions (version-region versions :start start :end end))
         (last (last versions)))
    (when (and start (version< start (first versions)))
      (push start versions))
    (when (and end (version< (car last) end))
      (setf (cdr last) (list end)))
    versions))

(defmethod last-known-system-version (system)
  (config :versions (system-name system)))

(defmethod (setf last-known-system-version) (version system)
  (config :versions (system-name system) (encode-version version)))

(defun ensure-system (system-ish &optional parent)
  (typecase system-ish
    (asdf:system
     system-ish)
    ((or string symbol)
     (asdf:find-system system-ish T))
    (cons
     (asdf/find-component:resolve-dependency-spec
      parent system-ish))))

(defun traverse-system-dependencies (system function)
  (let ((cache (make-hash-table :test 'eq)))
    (labels ((traverse (system)
               (unless (gethash system cache)
                 (setf (gethash system cache) T)
                 (dolist (subsystem (append (asdf:system-defsystem-depends-on system)
                                            (asdf:system-depends-on system)))
                   (traverse (ensure-system subsystem system)))
                 (funcall function system))))
      (traverse system))))

(defgeneric version-dependencies (system version))

(defmethod version-dependencies (system version)
  ())

(defgeneric migrate-versions (system from to))

(defmethod migrate-versions (system from to))

(defmacro define-version-migration ((system from to) &body body)
  (check-type system keyword)
  (check-type from (or keyword null))
  (check-type to keyword)
  `(defmethod migrate-versions ((,(gensym "SYSTEM") (eql ,system))
                                (,(gensym "FROM") (eql ,from))
                                (,(gensym "TO") (eql ,to)))
     ,@body))

(defmethod migrate-versions :before (system from to)
  (let ((deps (version-dependencies system from)))
    (flet ((migrate-dependency (dependency)
             (loop for (dep version) in deps
                   do (when (eql (ensure-system dependency)
                                 (ensure-system dep))
                        (migrate dependency T version))
                   finally (migrate dependency T T))))
      (traverse-system-dependencies system #'migrate-dependency))))

(defmethod migrate-versions :after (system from to)
  (setf (last-known-system-version system) to))

(defun versions (system)
  (sort (remove-duplicates
         (loop for method in (c2mop:generic-function-methods #'migrate-versions)
               for (sys from to) = (c2mop:method-specializers method)
               for matching = (and (null (method-qualifiers method))
                                   (typep sys 'c2mop:eql-specializer)
                                   (eql system (c2mop:eql-specializer-object sys)))
               when (and matching (typep from 'c2mop:eql-specializer))
               collect (c2mop:eql-specializer-object from)
               when (and matching (typep to 'c2mop:eql-specializer))
               collect (c2mop:eql-specializer-object to))
         :test #'version=)
        #'version<))

(defmethod migrate (system from to)
  (unless (version= from to)
    (assert (version< from to) (from to)
            "Cannot migrate backwards from ~s to ~s."
            (encode-version from) (encode-version to))
    (let ((versions (version-bounds (versions system) :start from :end to)))
      (loop for (from to) on versions
            do (migrate-versions system from to)))))

(defmethod migrate (system from (to (eql T)))
  (let ((version (asdf:component-version version)))
    (if version
        (migrate system from version)
        (error 'system-has-no-version :system system))))

(defmethod migrate (system (from (eql T)) to)
  (migrate system (last-known-system-version system) to))

(define-version-migration (:radiance-core NIL :2.0.0)
  (let ((previous-config-directory (merge-pathnames "radiance/" (ubiquitous:config-directory))))
    (when (uiop:directory-exists-p previous-config-directory)
      (loop for original-path in (uiop:subdirectories previous-config-directory)
            for environment = (car (last (pathname-directory environment)))
            for new-path = (environment-directory environment :configuration)
            do (rename-file original-path new-path)))))
