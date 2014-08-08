#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(define-hook server-start ())
(define-hook server-ready ())
(define-hook server-stop ())
(define-hook server-shutdown ())
(define-hook startup-done ())
(define-hook shutdown-done ())

(defvar *running* NIL)

(define-trigger startup ()
  (when *running*
    (error "Radiance is already running!"))
  (unless (implementation 'logger)
    (load-implementation 'logger))
  (l:info :radiance "Starting up.")
  (l:info :radiance "Ensuring prerequisites.")
  (unless (implementation 'server)
    (load-implementation 'server))

  (l:info :radiance "Starting server.")
  (trigger 'server-start)
  (setf *running* T)
  (trigger 'server-ready)

  (trigger 'startup-done)
  (l:info :radiance "Startup done.")
  T)

(define-trigger shutdown ()
  (unless *running*
    (error "Radiance is not running!"))
  (l:info :radiance "Stopping server.")
  (trigger 'server-stop)
  (setf *running* NIL)
  (trigger 'server-shutdown)

  (trigger 'shutdown-done)
  (l:info :radiance "Shutdown done.")
  T)
