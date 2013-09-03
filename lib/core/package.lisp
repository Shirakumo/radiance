#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(defpackage org.tymoonnext.radiance
  (:nicknames :org.tymoonnext.radiance :tynet-5 :tynet :radiance)
  (:use :cl :log4cl :cl-fad :lquery)
  (:export ;; conditions.lisp
           :radiance-error
           :module-already-initialized
           :error-page
           :auth-error
           :api-error
           :api-args-error
           :api-auth-error
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
           :effective-trigger
           :dispatch-default
           
           :user
           :user-get
           :user-field
           :user-save
           :user-saved-p
           :user-check
           :user-grant
           :user-prohibit
           :user-action
           :user-get-actions

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

           :profile
           :profile-field
           :profile-avatar
           :profile-name
           :profile-page-settings
           :profile-page-user
           :define-user-panel

           :database
           :db-connect
           :db-disconnect
           :db-connected-p
           :db-collections
           :db-create
           :db-empty
           :db-drop
           :db-select
           :db-iterate
           :db-insert
           :db-remove
           :db-update
           :db-apropos
           :query
           ::> ::< ::= ::>= ::<= ::in
           ::matches ::and ::or ::not

           :data-model
           :model-id
           :model-field
           :model-get
           :model-get-one
           :model-hull
           :model-hull-p
           :model-save
           :model-delete
           :model-insert
           :with-fields
           :with-model

           :admin
           :define-admin-panel
           
           :parser
           :parse
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
           ;; request.lisp
           :request
           :request-field
           ;; server.lisp
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
           :post-or-get-var
           :cookie-var
           :cookie-vars
           :header-var
           :header-vars
           :with-get
           :with-post
           :with-post-or-get
           :with-header
           :with-cookie
           :request-method
           :remote-address
           :redirect
           :get-redirect
           :static
           :template
           :read-data-file
           :error-page
           :upload-file
           :with-uploaded-file
           :defpage
           :define-file-link
           :link
           :defapi
           :api-return
           :api-format
           :define-api-format
           ;; toolkit.lisp
           :time-spent
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
           :hook-field
           :add-namespace
           :get-namespace-map
           :get-namespace
           :get-triggers
           :get-hooks
           :trigger
           ;; uri.lisp
           :uri
           :subdomains
           :domain
           :port
           :path
           :pathregex
           :uri-matches
           :uri-same
           :uri->url
           :uri->server-url
           :uri->context-url
           :make-uri
           ;; validate.lisp
           :username-p
           :displayname-p
           :url-p
           :email-p
           :date-p
           :date-to-timestamp
           :timestamp-to-date
           :timestamp-to-datetime
           )
  (:shadow :restart))

(in-package :radiance)

