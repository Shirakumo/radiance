#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defimpl dispatcher
    "Primary dispatcher module."
  (dispatch (request) "Dispatch a new webserver call.")
  (register (trigger) "Register a trigger to dispatch to."))
