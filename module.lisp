#|
 This file is a part of Radiance
 (c) 2014 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module-extension (radiance radiance-web)
  (:nicknames #:org.tymoonnext.radiance.lib.radiance.web)
  ;; api.lisp
  (:export
   #:*default-api-format*
   #:*serialize-fallback*
   #:api-output
   #:api-format
   #:remove-api-format
   #:define-api-format
   #:api-serialize
   
   #:api-option
   #:remove-api-option
   #:define-api-option

   #:api-page
   #:remove-api-page
   #:api-call
   #:make-api-call
   #:*api-body*
   #:define-api)
  ;; conditions.lisp
  (:export
   #:*debugger*
   #:radiance-error
   #:message
   #:radiance-warning
   #:internal-error
   #:request-error
   #:current-request
   #:request-empty
   #:request-not-found
   #:request-denied
   #:api-error
   #:api-auth-error
   #:api-argument-missing
   #:argument
   #:api-argument-invalid
   #:api-call-not-found
   #:api-response-empty
   #:api-unknown-format
   #:requested-format
   #:database-error
   #:database-warning
   #:database-connection-failed
   #:database
   #:database-connection-already-open
   #:database-invalid-collection
   #:collection
   #:database-collection-already-exists
   #:database-invalid-field
   #:fielddef
   #:data-model-not-inserted-yet
   #:model
   #:user-error
   #:user
   #:user-not-found

   #:handle-condition)
  ;; continuation.lisp
  (:export)
  ;; default.lisp
  (:export
   #:favicon
   #:robots
   #:static
   #:welcome)
  ;; dispatch.lisp
  (:export
   #:uri-dispatcher
   #:remove-uri-dispatcher
   #:name
   #:dispatch-function
   #:priority
   
   #:uri-dispatcher
   #:make-uri-dispatcher
   #:define-uri-dispatcher
   #:list-uri-dispatchers
   #:dispatch)
  ;; init.lisp
  (:export
   #:server-start
   #:server-ready
   #:server-stop
   #:server-shutdown
   #:startup-done
   #:shutdown-done)
  ;; options.lisp
  (:export
   #:define-options-definer
   #:expand-options)
  ;; page.lisp
  (:export
   #:page-option
   #:remove-page-option
   #:define-page-option
   #:*page-body*
   #:define-page)
  ;; request.lisp
  (:export
   #:*request*
   #:*response*
   #:*session*
   #:*default-external-format*
   #:*default-content-type*
   
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

   #:cookie
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
   #:remove-route
   #:define-route
   #:define-matching-route
   #:define-target-route
   #:define-string-route
   #:route!)
  ;; toolkit.lisp
  (:export
   #:static-file
   #:template
   #:with-model
   #:with-model-fields
   #:do-models
   #:with-actions
   #:or*)
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
   #:merge-uris
   #:uri-to-string))
