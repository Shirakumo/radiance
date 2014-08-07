#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(define-hook (:server :init) (:documentation "Initializer plaster database table.")
  ;; PID: -1 or _id of annotated parent.
  ;; Type: Has to be a type-name in plaster-types.
  ;; View: 0 Public, 1 Unlisted, 2 Private, 3 Encrypted
  (db:create "plaster" '(("pid" :integer) ("title" :varchar 64) ("author" :varchar 32) ("type" :varchar 16)
                         ("time" :integer) ("text" :text) ("view" :integer) ("hits" :integer) ("ip" :varchar 4)) :indices '("pid" "author" "ip"))
  (db:create "plaster-types" '(("title" :varchar 16) ("name" :varchar 64) ("mime" :varchar 16)))
  (db:create "plaster-user" '(("user" :varchar 32) ("theme" :varchar 32) ("default-type" :varchar 16)) :indices '("user"))
  (db:create "plaster-themes" '(("title" :varchar 32) ("name" :varchar 32))))

(defun id->hash (id) (write-to-string id :base 36))

(defun hash->id (hash) (parse-integer hash :radix 36))

(uibox:define-fill-function id->hash (model field)
  (id->hash (uibox:parse-data field model)))

(uibox:define-fill-function type->mode (model field)
  (cdr (assoc "name" (first (db:select "plaster-types" (db:query (:= "mime" (uibox:parse-data field model))) :limit 1)) :test #'string-equal)))

(uibox:define-fill-function type->title (model field)
  (cdr (assoc "title" (first (db:select "plaster-types" (db:query (:= "mime" (uibox:parse-data field model))) :limit 1)) :test #'string-equal)))

(defmacro string-or (default &rest values)
  (let ((var (gensym)) (def (gensym)))
    `(let* ((,def ,default)
            (,var (or ,@values "")))
       (if (= 0 (length ,var)) ,def ,var))))

(defun crlf->lf (string)
  (cl-ppcre:regex-replace-all (format NIL "~C~C" #\return #\linefeed) string (string #\linefeed)))

(defparameter *zalgo-regex* "[\xCC\xCD]")
(defun purify-ascii (string)
  ;;(cl-ppcre:regex-replace-all *zalgo-regex* string "")
  string)

(defun encrypt (text password)
  (let ((salt (or (config-tree :plaster :encrypt-salt)
                  "gHjjaL213adjz9AC")))
    (when (< (length text) 16)
      (setf text (concatenate 'string text (make-string (- 16 (length text)) :initial-element #\Space))))
    (concatenate
     'string
     (radiance-crypto:simple-hash text salt :iterations 1 :digest 'ironclad:md5) "-"
     (radiance-crypto:encrypt text (radiance-crypto:pbkdf2-key password salt :digest :sha256 :iterations 1)))))

(defun decrypt (text password)
  (destructuring-bind (hash text) (split-sequence:split-sequence #\- text)
    (let* ((salt (or (config-tree :plaster :encrypt-salt)
                     "gHjjaL213adjz9AC"))
           (decrypted (radiance-crypto:decrypt text (radiance-crypto:pbkdf2-key password salt :digest :sha256 :iterations 1)))
           (hashed (radiance-crypto:simple-hash decrypted salt :iterations 1 :digest 'ironclad:md5)))
      (when (string-equal hashed hash)
        decrypted))))

(defun paste-accessible-p (paste &optional (user (user:current :authenticate T)))
  (and paste
       (or (and (or (not (= (dm:field paste "view") 2))
                    (and user (string-equal (user:field user "username") (dm:field paste "author"))))
                (or (not (= (dm:field paste "view") 3))
                    (and (server:post-or-get "password")
                         (< 0 (length (server:post-or-get "password")))
                         ;; We've come this far, decrypt it and set it so we don't have to do it twice.
                         (setf (getdf paste "text")
                               (decrypt (dm:field paste "text") (server:post-or-get "password")))))
                ;; View permissions cascade from parent, so check it.
                (or (= (dm:field paste "pid") -1)
                    (paste-accessible-p (dm:get-one "plaster" (db:query (:= "_id" (dm:field paste "pid")))) user)))
           (and user (user:check "plaster.admin.*" :user user)))))

(defparameter *captcha-salt* (make-random-string))
(defparameter *captchas* '("divisible" "determined" "questionable" "difficult" "simplistic" "always" "never" "however" "occasionally" "certainly"
                           "creative" "video" "games" "whatever" "realistic" "severe" "explosion" "wizard" "witch" "confederation"
                           "united" "guess" "estimate" "uncertainty" "forgetful" "loathing" "nevermind" "incorrect" "detective" "deduction"
                           "reasoning" "evidence" "incident" "curiosity" "thoughtful" "assemble" "story" "conclusion" "possibility" "culprit"
                           "solved" "probability" "equation" "careful" "consider" "detail" "problematic" "complication" "comparison" "doubt"))
(defun generate-captcha ()
  (let* ((el (random-elt *captchas*))
         (elmix (copy-seq el)))
    (loop for i from 0 below 2
          do (setf (elt elmix (+ (random (- (length el) 2)) 1)) #\-))
    (values
     elmix
     (radiance-crypto:pbkdf2-hash el *captcha-salt*))))

;; TODO: Take care of error reporting problems of api functions.

(core:define-page index #u"plaster./" () (server:redirect "/new"))

(core:define-page list #u"plaster./recent" (:lquery (template "plaster/list.html"))
  (uibox:fill-foreach (dm:get "plaster" (db:query (:= "view" 0) (:= "pid" -1)) :sort '(("time" . :DESC)) :limit 20) "#pastelist .paste")
  (uibox:fill-all "body" (user:current :authenticate T :default (user:get "temp"))))

(core:define-page user #u"plaster./user" (:lquery (template "plaster/user.html"))
  (destructuring-bind (path username &optional (page "0")) (split-sequence:split-sequence #\/ (path *radiance-request*))
    (declare (ignore path))
    (setf username (string-downcase username))
    (let ((user (user:current :authenticate T :default (user:get "temp")))
          (viewuser (user:get username))
          (page (or (parse-integer page :junk-allowed T) 1)))
      (if viewuser
          (progn
            ($ "head title" (text (format NIL "~a's Profile - Plaster" (profile:name viewuser))))
            (uibox:fill-all "#userinfo" viewuser)
            (if (and (string= (user:field user "username") (user:field viewuser "username"))
                     (not (string= (user:field viewuser "username") "temp")))
                (progn
                  (uibox:fill-foreach (dm:get "plaster" (db:query (:= "pid" -1) (:= "author" username))
                                              :sort '(("time" . :DESC))) "#pastelist .paste")
                  (uibox:fill-foreach (dm:get "plaster" (db:query (:!= "pid" -1) (:= "author" username))
                                              :sort '(("time" . :DESC))) "#annotatelist .paste"))
                (progn
                  (uibox:fill-foreach (dm:get "plaster" (db:query (:= "pid" -1) (:= "view" 0) (:= "author" username))
                                              :sort '(("time" . :DESC)) :limit 20 :skip (* 20 (1- page))) "#pastelist .paste")
                  (uibox:fill-foreach (dm:get "plaster" (db:query (:!= "pid" -1) (:= "view" 0) (:= "author" username))
                                              :sort '(("time" . :DESC)) :limit 20 :skip (* 20 (1- page))) "#annotatelist .paste"))))
          ($ "#content" (html "<h2>No such user found.</h2>")))
      (uibox:fill-all "body" user))))

(core:define-page new #u"plaster./new" (:lquery (template "plaster/new.html"))
  (let* ((user (user:current :authenticate T :default (user:get "temp")))
         (annotate (when-let ((annotate-id (server:get "annotate")))
                     (dm:get-one "plaster" (db:query (:= "_id" (hash->id annotate-id))
                                                     (:= "pid" -1)))))
         (repaste (when-let ((repaste-id (server:get "repaste")))
                    (dm:get-one "plaster" (db:query (:= "_id" (hash->id repaste-id))))))
         ;; We have to do this here due to paste-accessible-p's side-effecting decryption.
         (accessible (or (and (not annotate) (not repaste))
                         (paste-accessible-p (or annotate repaste) user)))
         (text (crlf->lf (or (server:post "text")
                             (when annotate (dm:field annotate "text"))
                             (when repaste (dm:field repaste "text"))
                             "")))
         (type (or (server:post "type")
                   (when annotate (dm:field annotate "type"))
                   (when repaste (dm:field repaste "type"))))
         (title (or (server:post "title") ""))
         (view (server:post "view"))
         (password (or (server:post-or-get "password")
                       ""))
         (err (when (string= (server:post "action") "paste")
                (handler-case (core:api-call "plaster/paste" :POST text :annotate (server:post "annotate") :title title :type type
                                                             :view view :captcha (server:post "captcha") :hash (server:post "hash")
                                                             :password password :client "true")
                  (radiance-error (err) (text err))))))

    (if accessible
        (if (or (config-tree :plaster :anon) (not (string-equal (user:field user "username") "temp")))
            (progn
              (uibox:fill-foreach (dm:get "plaster-types" :all :sort '(("title" . :ASC))) "#typeselect option")
              ($ ".code" (text text))
              ($ (inline (format NIL "#viewselect option[value=\"~a\"]" view)) (attr :selected "selected"))
              ($ "#title" (val title))
              ($ "#viewpassword" (val password))
              (when-let ((model (dm:get-one "plaster-user" (db:query (:= "user" (user:field user "username"))))))
                ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme"))))
                (unless type
                  (setf type (dm:field model "default-type"))))
              (if (string-equal (user:field user "username") "temp")
                  (progn
                    (if (config-tree :plaster :captcha) 
                        (multiple-value-bind (captcha hash) (generate-captcha)
                          ($ "#captcha input[name=\"captcha\"]" (val captcha))
                          ($ "#captcha input[name=\"hash\"]" (val hash)))
                        ($ "#captcha" (remove)))
                    ($ "#viewselect option[value=2]" (remove)))
                  ($ "#captcha" (remove)))
              (when annotate
                ($ "#annotateinfo" (text (format NIL "Annotating paste ~a." (id->hash (dm:field annotate "_id")))))
                ($ "#viewselect" (parent) (replace-with "public/private depending on its parent"))
                ($ "#annotateid" (attr :value (id->hash (dm:field annotate "_id")))))
              ($ (inline (format NIL "#typeselect option[value=\"~a\"]" (or type "text/plain"))) (attr :selected "selected")))
            ($ "#content" (html "<h2>Anonymous pasting is not permitted. Please log in first.</h2>")))
        ($ "#content" (html "<h2>You are not allowed to repaste/annotate this paste.</h2>")))
    (uibox:fill-all "body" user)
    (when err (uibox:notice err :type :error :prepend "#content"))))

(core:define-page view #u"plaster./view" (:lquery (template "plaster/view.html"))
  (let* ((user (user:current :authenticate T :default (user:get "temp")))
         (paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id (server:get "id")))
                                                (:= "pid" -1)))))
    (cond
      ((not paste)
       ($ "#content" (html "<h2>No such paste.</h2>")))
      ((not (paste-accessible-p paste user))
       (if (= (dm:field paste "view") 3)
           (progn
             ($ "#content" (html-file (template "plaster/passwordprompt.html")))
             (uibox:fill-all "body" paste))
           ($ "#content" (html "<h2>You are not allowed to view this paste.</h2>"))))
      (T
       ($ "head title" (text (format NIL "~a - Paste #~a - Plaster" (dm:field paste "title") (id->hash (dm:field paste "_id")))))
       (uibox:fill-all "#maineditor" paste)
       (unless (or (user:check "plaster.admin.*" :user user)
                   (and (string-equal (dm:field paste "author") (user:field user "username"))
                        (not (string-equal (user:field user "username") "temp"))))
         ($ "#maineditor .editorbar .edit" (remove)))
       (uibox:fill-foreach
        (dm:get "plaster" (db:query (:= "pid" (dm:field paste "_id"))) :sort '(("time" . :ASC)))
        "#annotations .annotation"
        :iter-fun #'(lambda (model node)
                      (when (= (dm:field model "view") 3)
                        (setf (getdf model "text") (decrypt (dm:field model "text") (server:get "password"))))
                      (unless (and (string-equal (dm:field model "author") (user:field user "username"))
                                   (not (string-equal (user:field user "username") "temp")))
                        ($ node ".editorbar .edit" (remove)))))
       (when (= (dm:field paste "view") 3)
         ($ ".editorbar button" (each #'(lambda (node) ($ node (attr :formaction (format NIL "~a&password=~a" ($ node (attr :formaction) (node)) (server:get "password"))))))))
       (when-let ((model (dm:get-one "plaster-user" (db:query (:= "user" (user:field user "username"))))))
         ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme")))))
       (db:update "plaster" (db:query (:= "_id" (dm:field paste "_id"))) `(("hits" . ,(1+ (dm:field paste "hits")))))))
    (uibox:fill-all "body" user)))

(core:define-page edit #u"plaster./edit" (:lquery (template "plaster/edit.html"))
  (let* ((user (user:current :authenticate T))
         (paste (dm:get-one "plaster" (db:query (:= "_id" (hash->id (server:get "id")))))))
    (cond
      ((not paste)
       ($ "#content" (html "<h2>No such paste found.</h2>")))
      ((or (not user) (and (not (string-equal (dm:field paste "author") (user:field user "username")))
                           (not (user:check "plaster.admin.*" :user user))))
       ($ "#content" (html "<h2>You are not allowed to view this paste.</h2>")))
      (T
       (if (not (paste-accessible-p paste user))
           (progn
             ($ "#content" (html-file (template "plaster/passwordprompt.html")))
             (uibox:fill-all "body" paste))
           (let* ((text (or (server:post "text") (dm:field paste "text")))
                  (title (or (server:post "title") (dm:field paste "title")))
                  (type (or (server:post "type") (dm:field paste "type")))
                  (view (or (server:post "view") (dm:field paste "view")))
                  (password (server:post-or-get "password"))
                  (err (handler-case
                           (cond ((string= (server:post "action") "edit")
                                  (core:api-call "plaster/paste" :PATCH (server:get "id") :text text :title title :type type
                                                                 :view view :password password :client "true"))
                                 ((string= (server:post "action") "delete")
                                  (core:api-call "plaster/paste" :DELETE (server:get "id") :password password :client "true")))
                         (radiance-error (err) (text err)))))
             (when-let ((model (dm:get-one "plaster-user" (db:query (:= "user" (user:field user "username"))))))
               ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme")))))
             (uibox:fill-foreach (dm:get "plaster-types" :all :sort '(("title" . :ASC))) "#typeselect option")
             (uibox:fill-all "body" (user:get (dm:field paste "author")))
             ($ "#title" (attr :value title))
             ($ (inline (format NIL "#typeselect option[value=\"~a\"]" type)) (attr :selected "selected"))
             ($ (inline (format NIL "#viewselect option[value=\"~a\"]" view)) (attr :selected "selected"))
             ($ "#viewpassword" (attr :value password))
             ($ "#editid" (attr :value (id->hash (dm:field paste "_id"))))
             ($ ".code" (text text))
             (when err (uibox:notice err :type :error :prepend "#content"))))))
    (uibox:fill-all "body" user)))
