#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.test)

(define-test radiance
  (unless (radiance:started-p)
    (radiance:startup))
  (setf (v:repl-categories) NIL))

(define-test interfaces
  :parent radiance)
