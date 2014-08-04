#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module-extension (radiance radiance-web)
  (:nicknames #:org.tymoonnext.radiance.lib.radiance.web)
  ;; api.lisp
  (:export)
  ;; conditions.lisp
  (:export
   #:radiance-error
   #:message
   #:internal-error
   #:request-error
   #:current-request
   #:request-empty
   #:api-error
   #:api-argument-missing
   #:api-argument-invalid
   #:api-call-not-found
   #:api-response-empty
   #:api-unknown-format

   #:handle-condition)
  ;; continuation.lisp
  (:export)
  ;; dispatch.lisp
  (:export
   #:uri-dispatcher
   #:dispatch-function
   
   #:uri-dispatcher
   #:make-uri-dispatcher
   #:define-uri-dispatcher
   #:dispatch)
  ;; init.lisp
  (:export)
  ;; page.lisp
  (:export)
  ;; request.lisp
  (:export
   #:*request*
   #:*response*
   #:*default-external-format*
   
   #:request
   #:http-method
   #:headers
   #:post-data
   #:get-data
   #:cookies
   #:user-agent
   #:referer
   #:remote
   
   #:response
   #:data
   #:return-code
   #:content-type
   #:external-format
   #:headers
   #:cookies

   #:name
   #:value
   #:domain
   #:path
   #:expires
   #:http-only
   #:secure

   #:cookie
   #:get-var
   #:post-var
   #:post/get
   #:header
   #:file
   #:redirect
   #:serve-file
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
  ;; toolkit.lisp
  (:export)
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
