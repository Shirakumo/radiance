#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(defapi raw (id (password "")) (:method T)
  "Returns the raw paste text in text/plain format."
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (progn (server:set-content-type "text/plain")
                   (dm:field paste "text"))
            (error 'api-auth-error :apicall "raw" :module "plaster" :code 403 :text "You are not allowed to view this paste."))
        (error 'api-error :apicall "raw" :module "plaster" :code 404 :text "No such paste found."))))

(defapi paste (id (password "")) (:method :GET)
  "Returns all available information about a paste."
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

(defapi paste (text (annotate "-1") (title "") (type "text") (view "0") (password "") (client "false")) (:method :POST)
  "Create a new paste"
  (v:info :TEST "ADAWDD ~a" password)
  (let ((user (user :authenticate T))
        (annotate (hash->id (or annotate "-1")))
        (title (string-or "Untitled" title))
        (type (string-or "text" type))
        (view (parse-integer (string-or "0" view)))
        (client (string-equal client "true")))
    (when (= annotate -1) (setf annotate NIL))
    
    (assert-api (:apicall "paste" :module "plaster" :code 400 :text)
      ((< 0 (length text))
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
        ((and password (< 0 (length password)))
         "Encrypted view mode requires a password.")
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
