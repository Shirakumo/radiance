(in-package :radiance-mod-votey)

;; Populate initial database
(unless (db-select T "votey-options" :all)
  (loop for option in '("Yes" "No" "Neither" "Both" "Ech")
     for i = 0 then (1+ i)
     do (db-insert T "votey-options"
                   (acons "id" i 
                   (acons "votes" 0 
                   (acons "title" option ()))))))
  

(defpage vote #u"vote./" (:lquery (template "votey.html"))
  (let ((options (model-get T "votey-options" :all)))
    (if options
        (uibox:fill-foreach options "#options")
        ($ "#error" (text "No options available!"))))

  (if (not (authenticated-p)) 
      ($ "input[type=\"submit\"]" (remove)))
  ($ "#error" (text (get-var "error")))
  ($ "#ok" (text (get-var "ok"))))

(defapi vote (option) (:access-branch "*")
  (let ((option (model-get-one T "votey-options" (:= "id" (parse-integer option)))))
    (if option
        (progn (setf (model-field option "votes") 
                     (1+ (model-field option "votes")))
               (model-save option)
               (redirect "/?ok=Vote registered!"))
        (redirect "/?error=No such option!"))))
