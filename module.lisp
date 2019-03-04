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
  (:shadow #:define-interface #:virtual-module)
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
   #:current-module
   #:list-modules)
  ;; re-export from hooks
  (:export
   #:hook
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
   #:define-interface-extension)
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
   #:api-endpoint
   #:remove-api-endpoint
   #:list-api-endpoints
   #:api-endpoint
   #:name
   #:handler
   #:argslist
   #:request-handler
   #:call-api-request
   #:call-api
   #:define-api
   #:api-error)
  ;; conditions.lisp
  (:export
   #:*debugger*
   #:radiance-condition
   #:message
   #:definition-for-shared-package
   #:system-has-no-version
   #:backwards-migration-not-allowed
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
   #:api-unknown-format
   #:interface-implementation-not-set
   #:unparsable-uri-string
   #:no-such-post-parameter
   #:post-parameter-not-a-file
   #:file-to-serve-does-not-exist)
  ;; defaults.lisp
  (:export
   #:||
   #:favicon
   #:robots
   #:static
   #:add-domain
   #:remove-domain
   #:domain
   #:virtual-module)
  ;; dispatch.lisp
  (:export
   #:uri-dispatcher
   #:name
   #:dispatch-function
   #:priority
   #:uri-dispatcher
   #:remove-uri-dispatcher
   #:list-uri-dispatchers
   #:uri-dispatcher>
   #:define-uri-dispatcher
   #:dispatch
   #:abort-handling)
  ;; documentable.lisp
  (:export
   #:documentable
   #:define-documentable)
  ;; environment.lisp
  (:export
   #:environment-change
   #:environment
   #:environment-directory
   #:environment-module-directory
   #:environment-module-pathname
   #:check-environment
   #:set-environment
   #:reload-environment
   #:mconfig-pathname
   #:mconfig-storage
   #:mconfig
   #:defaulted-mconfig
   #:remmconfig
   #:config
   #:defaulted-config
   #:remconfig
   #:static-file
   #:@static
   #:template-file
   #:@template)
  ;; handle.lisp
  (:export
   #:*debugger*
   #:maybe-invoke-debugger
   #:handle-condition
   #:render-error-page
   #:execute-request
   #:set-data
   #:request)
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
   #:define-resource-locator
   #:define-option-type)
  ;; interfaces.lisp
  (:export
   #:find-implementation
   #:load-implementation
   #:define-interface
   #:define-implement-trigger)
  ;; migration.lisp
  (:export
   #:last-known-system-version
   #:migrate-versions
   #:define-version-migration
   #:ready-dependency-for-migration
   #:ensure-dependencies-ready
   #:versions
   #:migrate)
  ;; modules.lisp
  (:export
   #:module-domain
   #:module-permissions
   #:module-dependencies
   #:module-required-interfaces
   #:module-required-systems
   #:module-pages
   #:module-api-endpoints
   #:describe-module
   #:find-modules-directory
   #:*modules-directory*
   #:create-module
   #:find-all-modules)
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
   #:remove-page
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
   #:request-run-time)
  ;; resource.lisp
  (:export
   #:resource-type
   #:name
   #:locators
   #:resource-type
   #:remove-resource-type
   #:list-resource-types
   #:resource-locator
   #:define-resource-type
   #:define-resource-locator
   #:call-default-locator
   #:resource
   #:domain
   #:api
   #:static
   #:page)
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
   #:or*
   #:perm
   #:parse-path-safely
   #:url-encode
   #:rewrite-url
   #:merge-url
   #:with-actions)
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
   #:uri-to-url
   #:make-url
   #:format-uri))

(define-module #:radiance-user
  (:nicknames #:rad-user)
  (:use #:cl #:radiance)
  (:import-from #:uiop #:quit))
