#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance
  (:nicknames :org.tymoonnext.radiance :tynet-5 :tynet :radiance)
  (:use :cl :log4cl :cl-fad)
  (:export ;; conditions.lisp
           :radiance-error
           :module-already-initialized
           :auth-error
           :api-error
           :api-args-error
           ;; globals.lisp
           :*radiance-startup-time*
           :*radiance-config-file*
           :*radiance-config*
           :*radiance-acceptors*
           :*radiance-handlers*
           :*radiance-request*
           :*radiance-request-count*
           :*radiance-request-total*
           :*radiance-reply*
           :*radiance-implements*
           :*radiance-modules*
           :*radiance-hooks*
           :*radiance-session*
           :*radiance-api-formats*
           :*random-string-characters*
           :*default-cookie-expire*
           :*unix-epoch-difference*
           ;; implement.lisp
           :implementation
           :implement
           :defimplclass
           :defimpl
           :implementation
           :load-implementations
           ;; interfaces.lisp
           :core

           :dispatcher
           :dispatch
           :register
           :unregister
           :dispatch-default
           
           :user
           :user-get
           :user-field
           :user-save
           :user-saved-p
           :user-check
           :user-grant
           :user-prohibit

           :auth
           :authenticate
           :auth-page-login
           :auth-page-logout
           :auth-page-register
           :auth-page-options

           :session
           :session-get
           :session-start
           :session-start-temp
           :session-uuid
           :session-user
           :session-field
           :session-end
           :session-active-p
           :session-temp-p

           :database
           :db-connect
           :db-disconnect
           :db-connected-p
           :db-collections
           :db-create
           :db-select
           :db-iterate
           :db-insert
           :db-remove
           :db-update
           :db-apropos

           :data-model
           :model-field
           :model-get
           :model-get-one
           :model-hull
           :model-hull-p
           :model-save
           :model-delete
           :model-insert
           :with-fields

           ::and ::or ::not ::in ::!in ::matches
           ::= ::< ::> ::<= ::>=
           
           :admin
           :site
           :admin-category
           :admin-panel
           ;; module.lisp
           :module
           :name
           :author
           :version
           :license
           :url
           :collections
           :persistent
           :implementations
           :asdf-system
           :dependencies
           :compiled-p
           :defmodule
           :init
           :shutdown
           :get-module
           :module-package
           :module-symbol
           :discover-modules
           :load-module
           :compile-module
           :compile-dependency
           :column
           :collection
           :make-column
           :make-collection
           ;; server.lisp
           :request
           :response
           :manage
           :server-running-p
           ;; site.lisp
           :authenticated-p
           :authorized-p
           :set-cookie
           :get-var
           :get-vars
           :post-var
           :post-vars
           :redirect
           :static
           :template
           :read-data-file
           :error-page
           :upload-file
           :with-uploaded-file
           :with-get
           :with-post
           :uri
           :subdomains
           :domain
           :port
           :path
           :pathregex
           :uri-matches
           :uri->url
           :make-uri
           :defpage
           :define-file-link
           :link
           :defapi
           :api-return
           :api-format
           :define-api-format
           ;; toolkit.lisp
           :load-config
           :config
           :config-tree
           :concatenate-strings
           :plist->hash-table
           :make-keyword
           :package-symbol
           :nappend
           :universal-to-unix-time
           :unix-to-universal-time
           :get-unix-time
           :make-random-string
           :getdf
           :file-size
           :assoc-all
           ;; trigger.lisp
           :hook
           :namespace
           :hook-function
           :fields
           :description
           :hook-equal
           :defhook
           :add-namespace
           :get-namespace-map
           :get-namespace
           :get-triggers
           :get-hooks
           :trigger
           )
  (:shadow :restart))

(in-package :radiance)

