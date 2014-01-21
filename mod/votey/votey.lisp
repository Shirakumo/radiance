#|
This file is a part of TyNETv5/Radiance
(c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance-mod-votey)

;; Populate initial database
(unless (db:select "votey-options" :all)
  (loop for option in '("Yes" "No" "Neither" "Both" "Ech")
     for i = 0 then (1+ i)
     do (db:insert "votey-options"
                   (acons "id" i 
                   (acons "votes" 0 
                   (acons "title" option ()))))))
  

(defpage vote #u"vote./" (:lquery (template "votey.html"))
  (let ((options (dm:get "votey-options" :all)))
    (if options
        (uibox:fill-foreach options "#options")
        ($ "#error" (text "No options available!"))))

  (if (not (authenticated-p)) 
      ($ "input" (remove)))
  ($ "#error" (text (server:get "error")))
  ($ "#ok" (text (server:get "ok"))))

(defapi vote (option) (:access-branch "*")
  (let ((option (dm:get-one "votey-options" (:= "id" (parse-integer option)))))
    (if option
        (progn (setf (dm:field option "votes") 
                     (1+ (dm:field option "votes")))
               (dm:save option)
               (server:redirect "/?ok=Vote registered!"))
        (server:redirect "/?error=No such option!"))))
