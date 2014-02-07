#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance
  (:nicknames :org.tymoonnext.radiance :tynet-5 :tynet :radiance)
  (:use :cl :cl-fad :lquery :alexandria :split-sequence)
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
           #:no-such-interface-component-error
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
  (:export #:*radiance-startup-time*
           #:*radiance-config-file*
           #:*radiance-config*
           #:*radiance-request*
           #:*radiance-response*
           #:*radiance-session*
           #:*radiance-request-count*
           #:*radiance-request-total*
           #:*radiance-modules*
           #:*radiance-package-map*
           #:*radiance-hooks*
           #:*radiance-api-formats*
           #:*radiance-continuation-lifetime*
           #:*uri-matcher*
           #:*random-string-characters*
           #:*default-cookie-expire*
           #:+unix-epoch-difference+)
  ;; interface.lisp
  (:export #:define-interface
           #:define-interface-extension
           #:define-interface-method
           #:define-interface-component-expander
           #:get-interface-component-expander
           #:interface-component-types
           #:effective-system
           #:with-interface)
  ;; module.lisp
  (:export #:radiance-module
           #:define-module
           #:module-name
           #:module-package
           #:module-identifier
           #:module-system
           #:context-module
           #:context-module-identifier)
  ;; server.lisp
  (:export #:handler
           #:server-running-p
           #:manage
           #:start
           #:stop
           #:restart-server
           #:status)
  ;; site.lisp
  (:export #:upload-file
           #:with-uploaded-file)
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
           #:walk-directory
           #:static
           #:template
           #:read-data-file
           #:error-page)
  ;; trigger.lisp
  (:export #:hook-item
           #:name #:item-namespace #:item-identifier #:item-function #:item-description
           #:hook-equal
           #:hook-equalp
           #:namespace-map
           #:define-namespace
           #:add-hook-item
           #:namespace
           #:remove-namespace
           #:hooks
           #:hook-items
           #:trigger
           #:define-hook
           #:remove-hook
           #:clear-hook-items)
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

