#|
 This file is a part of Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

(defun encode-version (version)
  (etypecase version
    (null NIL)
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
    (null NIL)
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
  (cond ((eql a b) T)
        ((or (null a) (null b)) NIL)
        (T (multiple-value-bind (a-parts b-parts) (ensure-versions-comparable a b)
             (loop for a in a-parts
                   for b in b-parts
                   always (version-part= a b))))))

(defun version< (a b)
  (cond ((eql a b) NIL)
        ((null a) T)
        ((null b) NIL)
        (T (multiple-value-bind (a-parts b-parts) (ensure-versions-comparable a b)
             (loop for a in a-parts
                   for b in b-parts
                   do (cond ((version-part= a b))
                            ((version-part< a b) (return T))
                            (T                   (return NIL)))
                   finally (return NIL))))))

(defun version<= (a b)
  (cond ((eql a b) T)
        ((null a) T)
        ((null b) NIL)
        (T (multiple-value-bind (a-parts b-parts) (ensure-versions-comparable a b)
             (loop for a in a-parts
                   for b in b-parts
                   do (cond ((version-part= a b))
                            ((version-part< a b) (return T))
                            (T                   (return NIL)))
                   finally (return T))))))

(defun version-region (versions &key start end)
  (loop for version in versions
        when (and (or (not start) (version<= start version))
                  (or (not end) (version<= version end)))
        collect version))

(defun version-bounds (versions &key (start NIL start-p) end)
  (when versions
    (let* ((versions (version-region versions :start start :end end))
           (last (last versions)))
      (when versions
        (when (and start-p (version< start (first versions)))
          (push start versions))
        (when (and end (version< (car last) end))
          (setf (cdr last) (list end))))
      versions)))

(defmethod last-known-system-version ((system asdf:system))
  (config :versions (asdf:component-name system)))

(defmethod last-known-system-version (system)
  (last-known-system-version (asdf:find-system system T)))

(defmethod (setf last-known-system-version) (version (system asdf:system))
  (setf (config :versions (asdf:component-name system)) (encode-version version)))

(defmethod (setf last-known-system-version) (version system)
  (setf (last-known-system-version (asdf:find-system system T)) version))

(defun ensure-system (system-ish &optional parent)
  (typecase system-ish
    (asdf:system
     system-ish)
    ((or string symbol)
     (asdf:find-system system-ish T))
    (cons
     (asdf/find-component:resolve-dependency-spec
      parent system-ish))))

(defgeneric migrate-versions (system from to))

(defmethod ready-dependency-for-migration (dependency system from)
  (declare (ignore system from))
  (handler-bind ((system-has-no-version #'continue))
    (migrate dependency T T)))

(defmethod ensure-dependencies-ready ((system asdf:system) from)
  (loop for spec in (append (asdf:system-defsystem-depends-on system)
                            (asdf:system-depends-on system))
        for dependency = (ensure-system spec system)
        do (when (typep dependency 'virtual-module)
             (ready-dependency-for-migration dependency system from))))

(defmethod migrate-versions :before (system from to)
  (l:debug :radiance.migration "Migrating ~a from ~a to ~a."
           (asdf:component-name system) (encode-version from) (encode-version to))
  (ensure-dependencies-ready system from))

(defmethod migrate-versions (system from to))

(defmethod migrate-versions :after (system from to)
  (setf (last-known-system-version system) to))

(defmacro define-version-migration (system (from to) &body body)
  (check-type system (or symbol string))
  (let ((from (etypecase from
                ((or null keyword) from)
                ((or symbol string) (intern (string-upcase from) :keyword))))
        (to (etypecase to
              (keyword to)
              ((or symbol string) (intern (string-upcase to) :keyword)))))
    `(defmethod migrate-versions ((,(gensym "SYSTEM") (eql (asdf:find-system ',system)))
                                  (,(gensym "FROM") (eql ,from))
                                  (,(gensym "TO") (eql ,to)))
       ,@body)))

(defmethod versions ((system asdf:system))
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

(defmethod migrate ((system asdf:system) from to)
  (unless (version= from to)
    (with-simple-restart (abort "Abort the migration.")
      (with-simple-restart (force-version "Treat the migration as having been successful.")
        (assert (version< from to) (from to)
                'backwards-migration-not-allowed
                :from from :to to)
        (l:info :radiance.migration "Migrating ~a from ~a to ~a."
                (asdf:component-name system) (encode-version from) (encode-version to))
        (let ((versions (version-bounds (versions system) :start from :end to)))
          (loop for (from to) on versions
                while to
                do (migrate-versions system from to))))
      (setf (last-known-system-version system) to))))

(defmethod migrate ((system asdf:system) from (to (eql T)))
  (let ((version (asdf:component-version system)))
    (if version
        (migrate system from version)
        (cerror "Don't migrate the system and continue."
                'system-has-no-version :system system))))

(defmethod migrate ((system asdf:system) (from (eql T)) to)
  (migrate system (last-known-system-version system) to))

(defmethod migrate ((system symbol) from to)
  (migrate (asdf:find-system system T) from to))

(defmethod migrate ((system string) from to)
  (migrate (asdf:find-system system T) from to))
