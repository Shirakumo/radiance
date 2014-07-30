#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module-extension (radiance radiance-web)
  (:nicknames #:org.tymoonnext.radiance.lib.radiance.web)
  ;; dispatch.lisp
  (:export
   #:uri-dispatcher
   #:dispatch-function
   
   #:uri-dispatcher
   #:make-uri-dispatcher
   #:define-uri-dispatcher
   #:dispatch)
  ;; request.lisp
  (:export
   #:*request*
   #:*response*
   #:*default-external-format*
   
   #:request
   #:headers
   #:http-method
   #:post-data
   #:get-data
   #:cookies
   #:remote
   
   #:response
   #:data
   #:headers
   #:content-type
   #:return-code
   #:external-format
   
   #:request)
  ;; routing.lisp
  (:export
   #:route
   #:domains
   #:port
   #:path
   #:transformer
   
   #:route
   #:route-uri
   #:define-route
   #:resolve-route)
  ;; uri.lisp
  (:export
   #:uri
   #:domains
   #:port
   #:path
   #:matcher
   
   #:make-uri
   #:parse-uri
   #:uri<
   #:uri>
   #:uri=
   #:uri-matches
   #:merge-uris))
