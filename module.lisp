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
   #:api
   #:*default-api-format*
   #:*serialize-fallback*
   #:api-format
   #:remove-api-format
   #:list-api-formats
   #:define-api-format
   #:api-output
   #:api-serialize
   #:api-page
   #:remove-api-page
   #:list-api-pages
   #:call-api-request
   #:call-api
   #:define-api)
  ;; conditions.lisp
  (:export
   #:*debugger*
   #:radiance-condition
   #:message
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
   #:api-argument-invalid
   #:api-call-not-found
   #:api-response-empty
   #:api-unknown-format
   #:interface-implementation-not-set
   #:unparsable-uri-string
   #:no-such-post-parameter
   #:post-parameter-not-a-file)
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
   #:with-trigger
   #:uri-groups
   #:access
   #:favicon
   #:robots
   #:static
   #:add-domain
   #:remove-domain
   #:internalizer
   #:externalizer
   #:virtual-module)
  ;; dispatch.lisp
  (:export
   #:uri-dispatcher
   #:name
   #:dispatch-function
   #:priority
   #:uri-dispatcher
   #:list-uri-dispatchers
   #:uri-dispatcher>
   #:define-uri-dispatcher
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
  ;; interface-components.lisp
  (:export
   #:define-hook
   #:define-hook-switch
   #:define-resource-locator)
  ;; interfaces.lisp
  (:export
   #:find-implementation
   #:load-implementation
   #:define-interface
   #:define-implement-hook)
  ;; modules.lisp
  (:export
   #:*modules-directory*
   #:module-domain
   #:module-permissions
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
   #:option
   #:option-type
   #:name
   #:expander
   #:option
   #:remove-option
   #:list-options
   #:define-option
   #:expand-options)
  ;; page.lisp
  (:export
   #:page
   #:define-page)
  ;; request.lisp
  (:export
   #:*request*
   #:*response*
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

   #:present-error-page
   #:handle-condition
   #:render-error-page

   #:request-run-time)
  ;; resource.lisp
  (:export
   #:resource-type
   #:remove-resource-type
   #:list-resource-types
   #:resource-locator
   #:define-resource-type
   #:define-resource-locator
   #:resource)
  ;; routing.lisp
  (:export
   #:route
   #:name
   #:direction
   #:priority
   #:translator
   #:route
   #:remove-route
   #:list-routes
   #:define-route
   #:define-matching-route
   #:define-target-route
   #:define-string-route
   #:internal-uri
   #:external-uri)
  ;; toolkit.lisp
  (:export
   #:*random-string-characters*
   #:universal-to-unix-time
   #:unix-to-universal-time
   #:get-unix-time
   #:format-relative-time
   #:format-clock-time
   #:format-machine-date
   #:format-human-date
   #:format-fancy-date
   #:format-time
   #:make-random-string
   #:file-size
   #:resolve-base
   #:static-file
   #:template
   #:or*
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
   #:ensure-uri
   #:copy-uri
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
