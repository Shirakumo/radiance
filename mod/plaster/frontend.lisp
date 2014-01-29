#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-plaster)

(define-hook (:server :init) (:documentation "Initializer plaster database table.")
  (db:create "plaster" '(("pid" :integer) ("title" :varchar 64) ("author" :varchar 32) ("type" :varchar 16) ("time" :integer) ("text" :text) :indices '("pid" "author")))
  (db:create "plaster-types" '(("title" :varchar 16) ("name" :varchar 16)))
  (db:create "plaster-user" '(("user" :varchar 32) ("theme" :varchar 32) ("default-type" :varchar 16)) :indices '("user")))

(defpage list #u"plaster./" (:lquery T)
  )

(defpage profile #u"plaster./profile" (:lquery T)
  )

(defpage new #u"plaster./new" (:lquery T)
  (let* ((user (user :authenticate T :default (user:get "temp")))
         (annotate (when-let ((annotate-id (server:get "annotate")))
                     (dm:get-one "plaster" (db:query (:= "_id" annotate-id)))))
         (code (or (server:post "code")
                   (when annotate (dm:field annotate "text"))
                   "")))
    (if (and (string-equal (server:post "action") "paste")
             (< 0 (length code)))
        (with-model model ("plaster" NIL)
          (setf (getdf model "pid") (or (when annotate (dm:field annotate "_id"))
                                        -1)
                (getdf model "title") (or (server:post "title") "")
                (getdf model "author") (user:field user "username")
                (getdf model "type") (or (server:post "type") "text")
                (getdf model "time") (get-unix-time)
                (getdf model "text") code)
          (dm:insert model)
          (server:redirect (format NIL "/view?id=~a" (dm:field model "_id"))))
        (progn
          ($ (initialize (template "plaster/edit.html")))
          (uibox:fill-foreach (dm:get "plaster-types" :all :sort '(("title" . :ASC))) "#typeselect option")
          (uibox:fill-all "body" user)
          ($ "#code" (text code))
          (let ((model (dm:get-one "plaster-user" (db:query (:= "user" (user:field user "username"))))))
            (if model
                (progn
                  ($ "#editorthemescript" (text (format NIL "window.mirrorTheme=\"~a\";" (dm:field model "theme"))))
                  ($ (inline (format NIL "#typeselect option[value=\"~a\"]" (dm:field model "default-type"))) (attr :selected "selected")))
                ($ "#typeselect option[value=\"text\"]" (attr :selected "selected"))))))))

(defpage view #u"plaster./view" (:lquery (template "plaster/view.html"))
  )

(defpage edit #u"plaster./edit" (:lquery (template "plaster/edit.html"))
  )
