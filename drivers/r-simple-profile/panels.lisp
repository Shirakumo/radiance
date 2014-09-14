#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:simple-profile)

(define-implement-hook profile
  (profile:define-panel index (:lquery (template "panel-index.ctml"))
      )

  (profile:define-panel fooo ()))
