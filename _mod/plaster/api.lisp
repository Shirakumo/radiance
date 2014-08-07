#|
 This file is a part of TyNETv5/Radiance
 (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(core:define-api raw (id &optional (password "")) (:method T)
  "Returns the raw paste text in text/plain format."
  (declare (ignore password))
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (progn (server:set-content-type "text/plain")
                   (dm:field paste "text"))
            (error 'api-auth-error :apicall "raw" :module "plaster" :code 403 :text "You are not allowed to view this paste."))
        (error 'api-error :apicall "raw" :module "plaster" :code 404 :text "No such paste found."))))

(core:define-api paste (id &optional (password "")) (:method :GET)
  "Returns all available information about a paste."
  (declare (ignore password))
  (let ((paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id))))))
    (if paste
        (if (paste-accessible-p paste)
            (core:api-return 200 "Paste information."
                             :data (plist->hash-table
                                    :id (id->hash (dm:field paste "_id"))
                                    :pid (id->hash (dm:field paste "pid"))
                                    :title (dm:field paste "title")
                                    :author (dm:field paste "author")
                                    :type (dm:field paste "type")
                                    :time (dm:field paste "time")
                                    :view (dm:field paste "view")
                                    :hits (dm:field paste "hits")
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

(core:define-api paste (text &optional (annotate "-1") (title "") (type "text") (view "0") (password "") (captcha "") (hash "") (client "false")) (:method :POST)
  "Create a new paste"
  (paste-add text annotate title type view password captcha hash client))

(core:define-api paste/add (text &optional (annotate "-1") (title "") (type "text") (view "0") (password "") (captcha "") (hash "") (client "false")) (:method T)
  "Create a new paste"
  (paste-add text annotate title type view password captcha hash client))

(core:define-api paste (id &optional text title type view password (client "false")) (:method :PATCH)
  "Edit an existing paste"
  (paste-edit id text title type view password client))

(core:define-api paste/edit (id &optional text title type view password (client "false")) (:method T)
  "Edit an existing paste"
  (paste-edit id text title type view password client))

(core:define-api paste (id &optional password (client "false")) (:method :DELETE)
  "Delete an existing paste"
  (paste-delete id password client))

(core:define-api paste/delete (id &optional password (client "false")) (:method T)
  "Delete an existing paste"
  (paste-delete id password client))

(defun paste-add (text annotate title type view password captcha hash client)
  (let ((user (user:current :authenticate T))
        (annotate (hash->id annotate))
        (title (string-or "Untitled" (purify-ascii title)))
        (type (string-or "text" type))
        (view (parse-integer (string-or "0" view)))
        (client (string-equal client "true"))
        (maxpastes (config-tree :plaster :maxpastes))
        (cooldown (config-tree :plaster :cooldown))
        (last-time (cdr (assoc "time" (first (db:select "plaster" (db:query (:= "ip" (server:remote-address))) :limit 1 :sort '(("time" . :DESC)))) :test #'string=))))
    (when (= annotate -1) (setf annotate NIL))
    
    (assert-api (:apicall "paste" :module "plaster" :code)
      ((< 0 (length text))
       400 :text "Text must be at least one character long.")
      ((and (< -1 view) (< view 4))
       400 :text "View must be between 0 and 3.")
      ((<= (length title) 64)
       400 :text "Title must be less than 65 characters long.")
      ((or user (config-tree :plaster :anon))
       403 :text"Anonymous pasting is not permitted.")
      ((or (not (= 2 view)) user)
       403 :text "Anonymous users cannot create private pastes.")
      ((db:select "plaster-types" (db:query (:= "mime" type)))
       400 :text "Invalid type specified.")
      ((or (not user) (not maxpastes) (< maxpastes 0) (< (db:count "plaster" (db:query (:= "author" (user:field user "username")))) maxpastes))
       400 :text(format NIL "Max paste limit of ~a exceeded." maxpastes))
      ((or (not cooldown) (not last-time) (< cooldown (- (get-unix-time) last-time)))
       429 :text(format NIL "Please wait ~d seconds between pastes." cooldown)))

    (when (and (not user) (config-tree :plaster :captcha))
      (assert-api (:apicall "paste" :module "plaster" :code)
        ((< 0 (length hash))
         400 :text "Captcha hash is missing.")
        ((string= hash (radiance-crypto:pbkdf2-hash captcha *captcha-salt*))
         403 :text "Incorrect captcha.")))
    
    (when annotate
      (setf annotate (dm:get-one "plaster" (db:query (:= "_id" annotate)
                                                     (:= "pid" -1))))
      (setf view (dm:field annotate "view"))
      (assert-api (:apicall "paste" :module "plaster" :code)
        ((not (null annotate))
         404 :text "No such paste to annotate found.")
        ((paste-accessible-p annotate user)
         403 :text "You are not allowed to repaste/annotate this paste.")))

    (when (= view 3)
      (assert-api (:apicall "paste" :module "plaster" :code 400 :text)
        ((and password (< 5 (length password)))
         "Encrypted view mode requires a password of at least 6 characters.")
        ((<= (length password) 32)
         "Passwords must be less than 32 characters long."))
      (setf text (encrypt text password)))
    
    (when (and password (= (length password) 0)) (setf password NIL))
    (with-model model ("plaster" NIL)
      (setf (dm:field model "pid") (or (when annotate (dm:field annotate "_id")) -1)
            (dm:field model "title") title
            (dm:field model "author") (or (and user (user:field user "username")) "temp")
            (dm:field model "time") (get-unix-time)
            (dm:field model "text") text
            (dm:field model "view") view
            (dm:field model "type") type
            (dm:field model "hits") 0
            (dm:field model "ip") (server:remote-address))
      (dm:insert model)
      (if client
          (server:redirect (format NIL "/view?id=~a~@[&password=~a~]"
                                   (id->hash (dm:field (or annotate model) "_id")) password))
          (core:api-return 200 "Paste added." :data (plist->hash-table :id (dm:field model "_id")))))))

(defun paste-edit (id text title type view password client)
  (let ((user (user:current :authenticate T))
        (paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id)))))
        (client (string-equal client "true")))
    (setf view (if view (parse-integer view) (dm:field paste "view")))
    
    (assert-api (:apicall "paste" :module "plaster")
      ((not (null paste))
       :code 404 :text "No such paste found.")
      ((and user (or (string-equal (dm:field paste "author") (user:field user "username"))
                     (user:check "plaster.admin.*" :user user)))
       :code 403 :text "You are not allowed to edit this paste.")
      ((or (not text) (< 0 (length text)))
       :code 400 :text "Text must be at least one character long.")
      ((or (not view) (and (< -1 view) (< view 4)))
       :code 400 :text "View must be between 0 and 3.")
      ((db:select "plaster-types" (db:query (:= "mime" type)))
       :code 400 :text "Invalid type specified."))

    (when (= view 3)
      (assert-api (:apicall "paste" :module "plaster" :code 400 :text)
        ((and password (< 5 (length password)))
         "Encrypted view mode requires a password of at least 6 characters.")
        ((<= (length password) 32)
         "Passwords must be less than 32 characters long.")
        (text
         "Text to encrypt is required."))
      (setf text (encrypt text password)))

    (setf (getdf paste "title") (string-or (dm:field paste "title") (purify-ascii title))
          (getdf paste "view") view
          (getdf paste "type") (or type (dm:field paste "type"))
          (getdf paste "text") (or text (dm:field paste "text")))
    (dm:save paste)
    (if client
        (server:redirect (format NIL "/view?id=~a~@[&password=~a~]"
                                 (if (= (dm:field paste "pid") -1) id (id->hash (dm:field paste "pid"))) password))
        (core:api-return 200 "Paste edited."))))

(defun paste-delete (id password client)
  (let ((user (user:current :authenticate T))
        (paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id id)))))
        (client (string-equal client "true")))
    (assert-api (:apicall "paeste" :module "plaster")
      ((not (null paste))
       :code 404 :text "No such paste found.")
      ((and user (or (string-equal (dm:field paste "author") (user:field user "username"))
                     (user:check "plaster.admin.*" :user user)))
       :code 403 :text "You are not allowed to edit this paste."))

    (dm:delete paste)
    (db:remove "plaster" (db:query (:= "pid" (dm:field paste "_id"))))
    (if client
        (server:redirect (format NIL "/view?id=~a~@[&password=~a~]"
                                 (if (= (dm:field paste "pid") -1) id (id->hash (dm:field paste "pid"))) password))
        (core:api-return 200 "Paste deleted."))))

(core:define-api user/settings () (:method :GET :access-branch "*")
  "View user settings."
  (let ((prefs (dm:get-one "plaster-user" (db:query (:= "user" (user:field (user:current) "username"))))))
    (core:api-return 200 "User settings."
                     :data (plist->hash-table
                            :username (user:field (user:current) "username")
                            :theme (if prefs (dm:field prefs "theme") "default")
                            :default-type (if prefs (dm:field prefs "default-type") "text")))))

(core:define-api user/settings (&optional theme type nuke (client "false")) (:method :POST :access-branch "*")
  "Change user settings."
  (user-save theme type nuke client))

(core:define-api user/settings/save (&optional theme type nuke (client "false")) (:method T :access-branch "*")
  "Change user settings."
  (user-save theme type nuke client))

(defun user-save (theme type nuke client)
  (let ((username (user:field (user:current) "username"))
        (client (string-equal client "true")))
    (assert-api (:apicall "user/settings/save" :module "plaster" :code 400 :text)
      ((or (not theme) (db:select "plaster-themes" (db:query (:= "name" theme))))
       "Not a valid theme.")
      ((or (not type) (db:select "plaster-types" (db:query (:= "mime" type))))
       "Not a valid type."))
    
    (let ((prefs (dm:get-one "plaster-user" (db:query (:= "user" username)))))
      (when (null prefs)
        (setf prefs (dm:hull "plaster-user")
              (dm:field prefs "user") username))
      (when theme (setf (dm:field prefs "theme") (server:post "theme")))
      (when type (setf (dm:field prefs "default-type") (server:post "type")))
      (if (dm:hull-p prefs)
          (dm:insert prefs)
          (dm:save prefs)))
    
    (when (and nuke (string= nuke "nuke"))
      (let ((count (db:count "plaster" (db:query (:= "author" username)))))
        (db:remove "plaster" (db:query (:= "author" username)))
        (when client
          (server:redirect (make-uri (format NIL "user./settings/plaster/preferences?notice=~a pastes deleted." count))))))

    (if client
        (server:redirect (make-uri "user./settings/plaster/preferences?notice=Preferences updated."))
        (core:api-return 200 "Preferences saved."))))

(core:define-api user/import (&optional service) (:method T :access-branch "*")
  "Check if importing with the specified service is available. No service lists all available."
  (if service
      (core:api-return 200 service
                       :data (plist->hash-table
                              service (not (null (config-tree :plaster :import (find-symbol (string-upcase service) :KEYWORD))))))
      (core:api-return 200 "Available imports"
                       :data (mapcar #'car (config-tree :plaster :import)))))

(core:define-api user/import/pastebin (username password &optional (client "false")) (:method T :access-branch "*")
  "Import pastes from pastebin."
  (flet ((request (where &rest params)
           (apply #'drakma:http-request where :external-format-in :utf-8 :external-format-out :utf-8 params)))
    (let ((apikey (config-tree :plaster :import :pastebin :apikey))
          (user (user:field (user:current) "username")))
      (assert-api (:apicall "user/import/pastebin" :module "plaster" :code 400 :text)
        ((not (null apikey))
         "Pastebin import not configured."))
      (let ((userkey (request "http://pastebin.com/api/api_login.php"
                              :method :post :parameters `(("api_dev_key" . ,apikey)
                                                          ("api_user_name" . ,username)
                                                          ("api_user_password" . ,password)))))
        (assert-api (:apicall "user/import/pastebin" :module "plaster" :code 400 :text)
          ((not (string-equal userkey "Bad API request, invalid api_dev_key"))
           "Pastebin import not configured.")
          ((not (string-equal userkey "Bad API request, invalid login"))
           "Invalid login.")
          ((not (string-equal userkey "Bad API request, account not active"))
           "Specified account is inactive.")
          ((not (string-equal (subseq userkey 0 (length "Bad API")) "Bad API"))
           "Internal API error."))

        (let ((pastes (request "http://pastebin.com/api/api_post.php"
                               :method :post :parameters `(("api_dev_key" . ,apikey)
                                                           ("api_user_key" . ,userkey)
                                                           ("api_results_limit" . "100")
                                                           ("api_option" . "list"))))
              (lquery:*lquery-master-document*)
              (success ())
              (adapted ())
              (failed ()))
          (assert-api (:apicall "user/import/pastebin" :module "plaster" :code 400 :text)
            ((not (string-equal (subseq userkey 0 (length "Bad API")) "Bad API"))
             "Internal API error."))

          ($ (initialize (format NIL "<data>~a</data>" pastes)))
          (dolist (node ($ "data" (children)))
            (v:debug :plaster.import.pastebin "Importing ~a" ($ node "paste_key" (text) (node)))
            (let ((content (request "http://pastebin.com/raw.php"
                                    :method :get :parameters `(("i" . ,($ node "paste_key" (text) (node)))))))
              (if (and content (not (string-equal content "Error, this is a private paste. If this is your private paste, please login to Pastebin first.")))
                  (with-model (model pid title author type time text view hits) ("plaster" NIL)
                    (let ((realtype (pastebin->type ($ node "paste_format_short" (text) (node)))))
                      (if realtype
                          (push (dm:field model "_id") success)
                          (progn (setf realtype "text/plain")
                                 (push (dm:field model "_id") adapted)))
                      (setf title (string-or "Untitled" ($ node "paste_title" (text) (node)))
                            pid -1
                            author user
                            type realtype
                            time ($ node "paste_date" (text) (node))
                            text content
                            view (parse-integer ($ node "paste_private" (text) (node)))
                            hits (parse-integer ($ node "paste_hits" (text) (node))))
                      (dm:insert model)))
                  (push ($ node "paste_key" (text) (node)) failed))))
          (if client
              (server:redirect (make-uri (format NIL "user./settings/plaster/preferences?notice=~a pastes imported, ~a type adapted, ~a failed."
                                                 (length success) (length adapted) (length failed))))
              (core:api-return 200 "A"
                               :data (plist->hash-table
                                      :success success
                                      :adapted adapted
                                      :failed failed))))))))
