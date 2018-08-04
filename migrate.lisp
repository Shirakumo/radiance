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

(defmethod last-known-module-version (module)
  (config :versions (module-name module)))

(defmethod (setf last-known-module-version) (version module)
  (config :versions (module-name module) (encode-version version)))

(defgeneric migrate-versions (module from to))

(defmethod migrate-versions (module from to)
  :ignored)

(defmethod migrate-versions :after (module from to)
  (setf (last-known-module-version module) to))

(defun versions (module)
  (sort (remove-duplicates
         (loop for method in (c2mop:generic-function-methods #'migrate-versions)
               for (mod from to) = (c2mop:method-specializers method)
               for matching = (and (null (method-qualifiers method))
                                   (typep mod 'c2mop:eql-specializer)
                                   (eql module (c2mop:eql-specializer-object mod)))
               when (and matching (typep from 'c2mop:eql-specializer))
               collect (c2mop:eql-specializer-object from)
               when (and matching (typep to 'c2mop:eql-specializer))
               collect (c2mop:eql-specializer-object to))
         :test #'version=)
        #'version<))

(defmethod migrate (module from to)
  (let ((versions (version-bounds (versions module) :start from :end to)))
    (loop for (from to) on versions
          do (migrate-versions module from to))))

(defmethod migrate (module from (to (eql T)))
  (let* ((virtual (virtual-module module))
         (version (or (module-storage module :version)
                      (when virtual (asdf:component-version version)))))
    (if version
        (migrate module from version)
        (error 'module-has-no-version :module module))))

(defmethod migrate (module (from (eql T)) to)
  (migrate module (last-known-module-version module) to))
