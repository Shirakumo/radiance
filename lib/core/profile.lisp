#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(defun time-last-request (&key (iterations 100) (request *last-ht-request*) (reply *last-ht-reply*))
  (log:config :error)
  (format T "Last request: ~a~%" request)
  (with-timing iterations
    (handler request reply))
  (log:config :info))
 
(defmacro with-timing (iterations &body body)
  `(loop with realt = 0
      with runt = 0
      for i from 1 upto ,iterations
      do (multiple-value-bind (result real run) (time-spent ,@body)
           (declare (ignore result))
           (incf realt real)
           (incf runt run))
      finally (format T "Iterations: ~d~%~
                         Total real time: ~f~%~
                         Total run time:  ~f~%~
                         Avg. real time:  ~f~%~
                         Avg. run time:   ~f" ,iterations realt runt (/ realt i) (/ runt i))))
