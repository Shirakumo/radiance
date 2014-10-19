#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:radiance-core
  (:use #:cl #:modularize #:modularize-interfaces #:modularize-hooks #:lambda-fiddle)
  (:nicknames #:radiance #:org.tymoonnext.radiance.lib.radiance.core)
  (:shadow #:define-interface #:module)
  ;; re-export from modularize
  (:export
   #:define-module
   #:define-module-extension
   #:delete-module
   #:module
   #:module-p
   #:module-storage
   #:module-storage-remove
   #:module-identifier
   #:module-name
   #:current-module)
  ;; re-export from hooks
  (:export
   #:define-hook
   #:remove-hook
   #:define-trigger
   #:remove-trigger
   #:trigger)
  ;; re-export from interfaces
  (:export
   #:interface
   #:interface-p
   #:implementation
   #:reset-interface
   #:define-interface-extension
   #:defimpl
   #:i-defun
   #:i-defmacro
   #:i-defmethod)
  ;; accessor.lisp
  (:export
   #:field)
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
   #:*startup-time*
   #:uptime
   #:server-start
   #:server-ready
   #:server-stop
   #:server-shutdown
   #:startup
   #:startup-done
   #:shutdown
   #:shutdown-done)
  ;; interfaces.lisp
  (:export
   #:define-interface
   #:interface-implementation-not-set
   #:find-implementation
   #:load-implementation
   #:define-implement-hook)
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

   #:cookie-header
   #:cookie
   #:get-var
   #:post-var
   #:post/get
   #:header
   #:file
   #:redirect
   #:serve-file
   #:request
   #:set-data)
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
   #:*config-type*
   #:*root*
   #:*config-path*
   #:*data-path*
   #:load-config
   #:save-config
   #:config-tree
   #:make-keyword
   #:concatenate-strings
   #:universal-to-unix-time
   #:unix-to-universal-time
   #:get-unix-time
   #:make-random-string
   #:file-size
   #:read-data-file
   #:data-file
   #:resolve-base
   #:create-module
   #:static-file
   #:template
   #:with-model
   #:with-model-fields
   #:do-models
   #:with-actions
   #:or*
   #:cut-get-part
   #:format-universal-time)
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

(define-module #:radiance-user
  (:nicknames #:rad-user)
  (:use #:cl #:radiance))
