#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:simple-profile)

(define-implement-hook admin
  (admin:define-panel account settings (:access () :lquery (template "account.ctml") :icon "fa-user" :tooltip "Change account information.")
    (let ((user (auth:current))
          (fields (dm:get 'simple-profile-fields (db:query :all))))
      (with-actions (error info)
          ((:save
            (ratify:perform-test
              :email (post-var "email"))
            (setf (user:field user "displayname") (post-var "displayname")
                  (user:field user "email") (post-var "email"))
            (dolist (field fields)
              (let ((val (post-var (dm:field field "name"))))
                (ratify:perform-test (find-symbol (string-upcase (dm:field field "type")) :keyword) val)
                (setf (user:field user (dm:field field "name"))
                      (if (or (not val) (string= val ""))
                          (dm:field field "default")
                          val))))
            (user:save user)
            (setf info "Account updated.")))
        (r-clip:process
         T
         :error error :info info
         :user user
         :fields fields))))

  (admin:define-panel profile settings (:access () :lquery (template "profile.ctml") :icon "fa-home" :tooltip "Configure your profile looks.")
    (let ((user (auth:current)))
      (with-actions (error info)
          ((:save
            (ratify:perform-combined-tests
              (:color (post-var "color"))
              (:property (post-var "background"))
              (:boolean (post-var "show-actions")))
            (setf (user:field user "simple-profile-color") (post-var "color")
                  (user:field user "simple-profile-background") (post-var "background")
                  (user:field user "simple-profile-actions") (post-var "show-actions"))
            (user:save user)
            (setf info "Profile updated.")))
        (r-clip:process
         T
         :info info :error error
         :user user))))

  (admin:define-panel fields users (:access (radiance admin users fields) :lquery (template "fields.ctml") :icon "fa-list" :tooltip "Set user profile fields.")
    (with-actions (error info)
        ((:add
          (db:insert 'simple-profile-fields `((name . ,(post-var "name")) (type . ,(post-var "type")) (default . ,(post-var "default")))))
         (:delete
          (dolist (name (or (post-var "selected[]") (list (post-var "name"))))
            (db:remove 'simple-profile-fields (db:query (:= 'name name))))))
      (r-clip:process
       T
       :error error
       :fields (dm:get 'simple-profile-fields (db:query :all))))))
