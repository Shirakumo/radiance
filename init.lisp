#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.tymoonnext.radiance.lib.radiance.web)

(define-hook server-ready ())
(define-hook server-shutdown ())
(define-hook startup-done ())
(define-hook shutdown-done ())

(define-trigger startup ()
  (unless (implementation 'logger)
    (load-implementation 'logger))
  (l:info :radiance "Starting up.")
  (l:info :radiance "Ensuring prerequisites.")
  (unless (implementation 'server)
    (load-implementation 'server))

  (l:info :radiance "Starting server.")
  ;; (server:start)
  (trigger 'server-ready)

  (trigger 'startup-done)
  (l:info :radiance "Startup done.")
  T)

(define-trigger shutdown ()
  (l:info :radiance "Stopping server.")
  ;; (server:stop)
  (trigger 'server-shutdown)

  (trigger 'shutdown-done)
  (l:info :radiance "Shutdown done.")
  T)
