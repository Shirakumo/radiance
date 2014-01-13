#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance
  (:nicknames :org.tymoonnext.radiance :tynet-5 :tynet :radiance)
  (:use :cl :cl-fad :lquery :alexandria)
  (:shadowing-import-from :alexandria :copy-stream :copy-file)
  ;; conditions.lisp
  (:export #:radiance-error
           #:text #:code
           #:module-already-initialized
           #:module
           #:interface-error
           #:interface
           #:no-such-interface-error
           #:no-interface-implementation-error
           #:interface-not-implemented-error
           #:no-such-interface-function-error
           #:error-page
           #:auth-error
           #:invalid
           #:var #:validator
           #:api-error
           #:module #:apicall
           #:api-args-error
           #:api-auth-error
           #:hook-error
           #:namespace-conflict-error
           #:namespace-not-found-error)
  ;; continuation.lisp
  (:export #:request-continuation
           #:id #:name #:timeout #:request #:continuation-function
           #:continuation #:continuations
           #:make-continuation
           #:clean-continuations
           #:clean-continuations-globally
           #:with-request-continuation)
  ;; globals.lisp
  (:export #:*last-ht-request*
           #:*last-ht-reply*
           #:*radiance-startup-time*
           #:*radiance-config-file*
           #:*radiance-config*
           #:*radiance-acceptors*
           #:*radiance-handlers*
           #:*radiance-request*
           #:*radiance-request-count*
           #:*radiance-request-total*
           #:*radiance-reply*
           #:*radiance-modules*
           #:*radiance-package-map*
           #:*radiance-hooks*
           #:*radiance-session*
           #:*radiance-api-formats*
           #:*radiance-continuation-lifetime*
           #:*uri-matcher*
           #:*random-string-characters*
           #:*default-cookie-expire*
           #:+unix-epoch-difference+)
  ;; interface.lisp
  (:export #:define-interface
           #:define-interface-function)
  ;; module.lisp
  (:export #:define-module
           #:module-name
           #:module-package
           #:module-identifier
           #:module-system
           #:get-module)
  ;; request.lisp
  (:export #:request
           #:request-field
           #:parse-request)
  ;; server.lisp
  (:export #:server-running-p
           #:manage
           #:start
           #:stop
           #:restart-server
           #:status)
  ;; site.lisp
  (:export #:get-var
           #:authorized-p
           #:user
           #:set-cookie
           #:get-vars
           #:post-var
           #:post-vars
           #:post-or-get-var
           #:cookie-var
           #:cookie-vars
           #:header-var
           #:header-vars
           #:request-method
           #:remote-address
           #:set-content-type
           #:redirect
           #:get-redirect
           #:static
           #:template
           #:read-data-file
           #:error-page
           #:upload-file
           #:api-return
           #:api-format
           #:with-post
           #:with-post-or-get
           #:with-header
           #:with-cookie
           #:with-uploaded-file
           #:defpage
           #:define-file-link
           #:defapi
           #:define-api-format
           #:save-to-db
           #:validate-and-save)
  ;; standard-interfaces.lisp
  (:export #:with-fields
           #:with-model)
  ;; toolkit.lisp
  (:export #:load-config
           #:config
           #:config-tree
           #:concatenate-strings
           #:plist->hash-table
           #:package-symbol
           #:universal-to-unix-time
           #:unix-to-universal-time
           #:get-unix-time
           #:make-random-string
           #:getdf
           #:file-size
           #:assoc-all
           #:walk-directory)
  ;; trigger.lisp
  (:export #:hook-item
           #:name #:item-namespace #:item-identifier #:item-function #:item-description
           #:hook-equal
           #:hook-equalp
           #:namespace-map
           #:define-namespace
           #:add-hook-item
           #:namespace
           #:hooks
           #:hook-items
           #:trigger
           #:define-hook)
  ;; uri.lisp
  (:export #:uri
           #:subdomains
           #:domain
           #:port
           #:path
           #:regex
           #:uri-matches
           #:uri-same
           #:uri->url
           #:uri->server-url
           #:uri->context-url
           #:make-uri)
  ;; validate.lisp
  (:export #:verify-multiple
           #:username-p
           #:displayname-p
           #:domain-p
           #:url-p
           #:email-p
           #:date-p
           #:date-to-timestamp
           #:timestamp-to-date
           #:timestamp-to-datetime))

