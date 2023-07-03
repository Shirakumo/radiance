(in-package #:org.shirakumo.radiance.test)

(defun clear-ban-list ()
  (loop for (ip duration) in (ban:list)
          do (ban:release ip))
    (is eq () (ban:list)))

(defmacro with-clear-ban-list (&body body)
  `(unwind-protect
       (progn
         (clear-ban-list)
         ,@body)
    (clear-ban-list)))

(define-test ban
  :parent interfaces
  (with-clear-ban-list
    ;; Test jailing
    (ban:jail "127.0.0.1")
    (ban:jail "127.0.0.2" :duration t)
    (ban:jail "127.0.0.3" :duration 42)
    (is eql t (ban:jail-time "127.0.0.1"))
    (is eql t (ban:jail-time "127.0.0.2"))
    (let ((end-time (+ (get-universal-time) 42)))
      (is eql end-time (ban:jail-time "127.0.0.3"))
      (is = 3 (length (ban:list))))
    ;; Test releasing
    (ban:release "127.0.0.2")
    (is eq nil (ban:jail-time "127.0.0.2"))
    (is = 2 (length (ban:list)))
    ;; Test banning again
    (ban:jail "127.0.0.2")
    (is eql t (ban:jail-time "127.0.0.2"))
    (is = 3 (length (ban:list)))))
