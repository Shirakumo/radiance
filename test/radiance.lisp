#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.test)

(defun run-test (&key (report 'parachute:plain))
  (v:with-muffled-logging ()
    (parachute:test 'radiance :report report)))

(define-test radiance
  (unless (radiance:started-p)
    (radiance:startup)))

(define-test interfaces
  :parent radiance)
