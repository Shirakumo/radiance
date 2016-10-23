((:interfaces
  (:cache . "r-simple-cache")
  (:logger . "i-verbose")
  (:user . "r-simple-users")
  (:data-model . "r-simple-model")
  (:profile . "r-simple-profile")
  (:database . "i-lambdalite")
  (:admin . "r-simple-admin")
  (:auth . "r-simple-auth")
  (:session . "r-simple-sessions")
  (:server . "i-hunchentoot")
  (:rate . "r-simple-rate"))
 (:lambdalite
  (:default . :dev)
  (:connections
   (:dev . "dev.lambdalite.db")))
 (:server
  (:domains . ("radiance" "localhost" "127.0.0.1" "::1"))
  (:instances . (((:port . 8080))))))
