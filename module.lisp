#|
 This file is a part of Radiance
 (c) 2014 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:modularize-user)
(define-module #:radiance-core
  (:use #:cl #:modularize #:modularize-interfaces #:modularize-hooks
        #:lambda-fiddle)
  (:nicknames #:radiance #:org.shirakumo.radiance.core)
  (:shadow #:define-interface #:module)
  ;; re-export from modularize
  (:export
   #:virtual-module
   #:virtual-module-name
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
   #:list-hooks
   #:define-hook
   #:remove-hook
   #:define-trigger
   #:remove-trigger
   #:trigger
   #:define-hook-switch)
  ;; re-export from interfaces
  (:export
   #:interface
   #:interface-p
   #:implementation
   #:implements
   #:reset-interface
   #:define-interface-extension
   #:defimpl
   #:i-defun
   #:i-defmacro
   #:i-defmethod)
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
   #:call-api-request
   #:call-api
   #:define-api)
  ;; conditions.lisp
  (:export
   #:*debugger*
   #:radiance-error
   #:message
   #:radiance-warning
   #:environment-not-set
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

   #:present-error-page
   #:handle-condition)
  ;; config.lisp
  (:export
   #:environment-change
   #:environment
   #:check-environment
   #:set-environment
   #:mconfig-pathname
   #:mconfig-storage
   #:mconfig
   #:defaulted-mconfig
   #:config
   #:defaulted-config)
  ;; convenience.lisp
  (:export
   #:with-model
   #:with-model-fields
   #:with-model-save
   #:do-models
   #:with-actions)
  ;; defaults.lisp
  (:export
   #:favicon
   #:robots
   #:static
   #:welcome
   #:add-domain
   #:remove-domain)
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
   #:shutdown-done
   #:started-p)
  ;; interfaces.lisp
  (:export
   #:define-interface
   #:interface-implementation-not-set
   #:find-implementation
   #:load-implementation
   #:define-implement-hook)
  ;; modules.lisp
  (:export
   #:*modules-directory*
   #:domain
   #:permissions
   #:module-dependencies
   #:module-required-interfaces
   #:module-required-systems
   #:module-pages
   #:module-api-endpoints
   #:describe-module
   #:find-modules-directory
   #:create-module)
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
   #:uri
   #:http-method
   #:headers
   #:post-data
   #:get-data
   #:cookies
   #:user-agent
   #:referer
   #:domain
   #:remote
   #:data
   #:issue-time
   
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
   #:execute-request
   #:set-data
   #:request

   #:request-run-time
   #:thread-run-time)
  ;; resource.lisp
  (:export
   #:implicit-resource-type-warning
   #:define-resource-type
   #:remove-resource-type
   #:define-resource-locator
   #:resource)
  ;; routing.lisp
  (:export
   #:route
   #:name
   #:route-type
   #:priority
   #:translator
   #:route
   #:remove-route
   #:define-route
   #:define-matching-route
   #:define-target-route
   #:define-string-route
   #:internal-uri
   #:external-uri)
  ;; toolkit.lisp
  (:export
   #:*root*
   #:*data-path*
   #:make-keyword
   #:universal-to-unix-time
   #:unix-to-universal-time
   #:get-unix-time
   #:make-random-string
   #:file-size
   #:read-data-file
   #:data-file
   #:resolve-base
   #:static-file
   #:template
   #:or*
   #:cut-get-part
   #:format-universal-time
   #:perm)
  ;; uri.lisp
  (:export
   #:uri
   #:domains
   #:port
   #:path
   #:matcher

   #:uri-string
   #:make-uri
   #:parse-uri
   #:uri<
   #:uri>
   #:uri=
   #:uri-matches
   #:merge-uris
   #:represent-uri
   #:uri-to-url))

(define-module #:radiance-user
  (:nicknames #:rad-user)
  (:use #:cl #:radiance))
