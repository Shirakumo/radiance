#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(define-api raw (id &optional (password "")) (:method T)
  "Returns the raw paste text in text/plain format."
  (declare (ignore password))
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (progn (server:set-content-type "text/plain")
                   (dm:field paste "text"))
            (error 'api-auth-error :apicall "raw" :module "plaster" :code 403 :text "You are not allowed to view this paste."))
        (error 'api-error :apicall "raw" :module "plaster" :code 404 :text "No such paste found."))))

(define-api paste (id &optional (password "")) (:method :GET)
  "Returns all available information about a paste."
  (declare (ignore password))
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (api-return 200 "Paste information."
                        (plist->hash-table
                         :id (id->hash (dm:field paste "_id"))
                         :pid (id->hash (dm:field paste "pid"))
                         :title (dm:field paste "title")
                         :author (dm:field paste "author")
                         :type (dm:field paste "type")
                         :time (dm:field paste "time")
                         :view (dm:field paste "view")
                         :text (dm:field paste "text")))
            (error 'api-auth-error :apicall "raw" :module "plaster" :code 403 :text "You are not allowed to view this paste."))
        (error 'api-error :apicall "raw" :module "plaster" :code 404 :text "No such paste found."))))

(defmacro assert-api (default-args &body forms)
  "Assert multiple things at once.
Each form should be of the following format:
 (ASSERTION-FORM &rest EXTRA-ARGS)"
  `(progn
     ,@(mapcar #'(lambda (form)
                   `(unless ,(car form) (error 'api-error ,@default-args ,@(cdr form))))
               forms)))

(define-api paste (text &optional (annotate "-1") (title "") (type "text") (view "0") (password "") (client "false")) (:method :POST)
  "Create a new paste"
  (paste-add text annotate title type view password client))

(define-api paste/add (text &optional (annotate "-1") (title "") (type "text") (view "0") (password "") (client "false")) (:method T)
  "Create a new paste"
  (paste-add text annotate title type view password client))

(define-api paste (id &optional text title type view password (client "false")) (:method :PATCH)
  "Edit an existing paste"
  (paste-edit id text title type view password client))

(define-api paste/edit (id &optional text title type view password (client "false")) (:method T)
  "Edit an existing paste"
  (paste-edit id text title type view password client))

(define-api paste (id &optional password (client "false")) (:method :DELETE)
  "Delete an existing paste"
  (paste-delete id password client))

(define-api paste/delete (id &optional password (client "false")) (:method T)
  "Delete an existing paste"
  (paste-delete id password client))

(defun paste-add (text annotate title type view password client)
  (let ((user (user :authenticate T))
        (annotate (hash->id annotate))
        (title (string-or "Untitled" title))
        (type (string-or "text" type))
        (view (parse-integer (string-or "0" view)))
        (client (string-equal client "true")))
    (when (= annotate -1) (setf annotate NIL))
    
    (assert-api (:apicall "paste" :module "plaster" :code 400 :text)
      ((< 1 (length text))
       "Text must be at least one character long.")
      ((not (null (dm:get-one "plaster-types" (db:query (:= "name" type)))))
       (format NIL "Type ~a not valid." type))
      ((and (< -1 view) (< view 4))
       "View must be between 0 and 3.")
      ((or (not (= 2 view)) user)
       "Anonymous users cannot create private pastes."))
    
    (when annotate
      (setf annotate (dm:get-one "plaster" (db:query (:= "_id" annotate)
                                                     (:= "pid" -1))))
      (setf view (dm:field annotate "view"))
      (assert-api (:apicall "paste" :module "plaster")
        ((not (null annotate))
         :code 404 :text "No such paste to annotate found.")
        ((paste-accessible-p annotate user)
         :code 403 :text "You are not allowed to repaste/annotate this paste.")))

    (when (= view 3)
      (assert-api (:apicall "paste" :module "plaster" :code 400 :text)
        ((and password (< (length password) 6))
         "Encrypted view mode requires a password of at least 6 characters.")
        ((<= (length password) 32)
         "Passwords must be less than 32 characters long."))
      (setf text (encrypt text password)))
    
    (when (and password (= (length password) 0)) (setf password NIL))
    (with-model model ("plaster" NIL)
      (setf (getdf model "pid") (or (when annotate (dm:field annotate "_id")) -1)
            (getdf model "title") title
            (getdf model "author") (or (and user (user:field user "username")) "temp")
            (getdf model "time") (get-unix-time)
            (getdf model "view") view
            (getdf model "type") type
            (getdf model "text") text)
      (dm:insert model)
      (server:redirect (format NIL (if client "/view?id=~a~@[&password=~a~]" "/api/plaster/paste?id=~a~@[&password=~a~]")
                               (id->hash (dm:field (or annotate model) "_id")) password)))))

(defun paste-edit (id text title type view password client)
  (let ((user (user :authenticate T))
        (paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id)))))
        (client (string-equal client "true")))
    (setf view (if view (parse-integer view) (dm:field paste "view")))
    
    (assert-api (:apicall "paeste" :module "plaster")
      ((not (null paste))
       :code 404 :text "No such paste found.")
      ((and user (string-equal (dm:field paste "author") (user:field user "username")))
       :code 403 :text "You are not allowed to edit this paste.")
      ((or (not text) (< 1 (length text)))
       :code 400 :text "Text must be at least one character long.")
      ((or (not type) (not (null (dm:get-one "plaster-types" (db:query (:= "name" type))))))
       :code 400 :text (format NIL "Type ~a not valid." type))
      ((or (not view) (and (< -1 view) (< view 4)))
       :code 400 :text "View must be between 0 and 3."))

    (when (= view 3)
      (assert-api (:apicall "paste" :module "plaster" :code 400 :text)
        ((and password (< (length password) 6))
         "Encrypted view mode requires a password of at least 6 characters.")
        ((<= (length password) 32)
         "Passwords must be less than 32 characters long.")
        (text
         "Text to encrypt is required."))
      (setf text (encrypt text password)))

    (setf (getdf paste "title") (or title (dm:field paste "title"))
          (getdf paste "view") view
          (getdf paste "type") (or type (dm:field paste "type"))
          (getdf paste "text") (or text (dm:field paste "text")))
    (dm:save paste)
    (server:redirect (format NIL (if client "/view?id=~a~@[&password=~a~]" "/api/plaster/paste?id=~a~@[&password=~a~]")
                             (id->hash (if (= (dm:field paste "pid") -1) id (dm:field paste "pid"))) password))))

(defun paste-delete (id password client)
  (let ((user (user :authenticate T))
        (paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id)))))
        (client (string-equal client "true")))
    (assert-api (:apicall "paeste" :module "plaster")
      ((not (null paste))
       :code 404 :text "No such paste found.")
      ((and user (string-equal (dm:field paste "author") (user:field user "username")))
       :code 403 :text "You are not allowed to edit this paste."))

    (dm:delete paste)
    (server:redirect (format NIL (if client "/view?id=~a~@[&password=~a~]" "/api/plaster/paste?id=~a~@[&password=~a~]")
                             (id->hash (if (= (dm:field paste "pid") -1) id (dm:field paste "pid"))) password))))
