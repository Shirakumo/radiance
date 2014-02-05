#|
  This file is a part of TyNETv5/Radiance
  (c) 2013 TymoonNET/NexT http://tymoon.eu (shinmera@tymoon.eu)
  Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package :radiance)

(define-interface core
  (define-page (name uri (&key access-branch lquery identifier) &body body)
      (:type :macro)
      (:documentation "Define a new page for a given module.
NAME has to be a symbol identifying the page call.

URI should be an instance of URI, which will be used to
identify if a given request matches for the page call. See
MAKE-URI for more.

ACCESS-BRANCH if supplied performs an automatic 
AUTH:AUTHENTICATED-P check on the supplied access branch. As a
consequence it might also invoke AUTH:AUTHENTICATE and modify
*RADIANCE-SESSION*.

LQUERY can be either a pathname to initialize lQuery with or T. 
In both cases, the defined page will automatically trigger the 
 (:user :lquery-post-processing) hook before finally returning
the result of ($ (serialize)). The return value of the body is
discarded. Both of these actions are not performed
if the response's content is not-NIL."))
  (define-api (name args (&key method access-branch identifier) &body body)
      (:type :macro)
      (:documentation "Define a new api function.
NAME has to be a symbol identifying the api call path:
 /api/IDENTIFIER/NAME

ARGS should be a lambda-list limited to the &optional, &key
and &aux operators. Note that due to the nature of HTTP
requests, the order of arguments and the distinction between
&optional and &key does not actually matter.

METHOD should be one of T, :GET, :POST, :PUT, :PATCH, :DELETE.
The same api function can be defined on multiple methods. In
the case of GET, only GET variables are considered, similarly
for POST. For all other functions, the POST takes precedence
over GET, but both are considered.

ACCESS-BRANCH if supplied performs an automatic authenticated-p
check on the supplied access branch. As a consequence it will
also invoke AUTH:AUTHENTICATE."))
  (define-api-format (name content-type datavar &body body)
      (:type :macro)
      (:documentation "Define a new API output format function. The body of this should return a string, which is then used as the API response."))
  (define-file-link (name uri pathspec &key content-type access-branch identifier)
    (:type :macro)
    (:documentation "Defines a link of a given URI to a file. Useful for static dispatching on files that cannot be in the static/ directory for one reason or another (e.g. favicon.ico, robots.txt)."))
  (api-format (format data)
    (:documentation "Invoke the requested api-format function as defined by DEFINE-API-FORMAT. Expects the data in form of a plist and the format as a keyword."))
  (api-return (code text &key data)
    (:documentation "Generates an API response in the proper format (:CODE code :TEXT text :TIME timestamp :DATA data).")))

(define-interface server
  (request ()
    (:type :class)
    (:superclasses (uri))
    (:documentation "Base class for the server's request object that contains information about the current request. 
An instance of this will be bound to *radiance-request* during dispatch."))
  (response ((content :initform NIL :accessor content))
    (:type :class)
    (:documentation "Base class for the server's response object that contains information about the response that will be sent out. 
An instance of this will be bound to *radiance-response* during dispatch."))
  (start-listener (name &key address port)
    (:documentation "Starts a new server listener that will dispatch requests to the set handler function."))
  (stop-listener (name)
    (:documentation "Shuts down the specified listener."))
  (get-listeners ()
    (:documentation "Returns a list of all listener names."))
  (cookie (name &key request)
    (:documentation "Returns the value of the requested cookie or NIL."))
  (cookies (&key request)
    (:documentation "Returns an alist of all cokies."))
  (get (name &key request)
    (:documentation "Returns the value of the requested GET variable or NIL."))
  (gets (&key request)
    (:documentation "Returns an alist of all GET variables."))
  (post (name &key request)
    (:documentation "Returns the value of the requested POST variable or NIL."))
  (posts (&key request)
    (:documentation "Returns an alist of all POST variables."))
  (post-or-get (name &key request)
    (:documentation "Returns the value of the requested POST or GET variable or NIL."))
  (header (name &key request-or-response)
    (:documentation "Returns the value of the requested header or NIL."))
  (headers (&key request-or-response)
    (:documentation "Returns an alist of all headers."))
  (request-method (&key request)
    (:documentation "Returns the request method used (GET/POST/DELETE/etc)."))
  (remote-address (&key request)
    (:documentation "Returns the remote address of the connecting client."))
  (remote-port (&key request)
    (:documentation "Returns the port of the connecting client."))
  (referer (&key request)
    (:documentation "Returns the HTTP Referer header."))
  (user-agent (&key request)
    (:documentation "Returns the HTTP User-Agent header."))
  (local-address (&key request)
    (:documentation "Returns the local address of the current request."))
  (local-port (&key request)
    (:documentation "Returns the local port of the current request."))
  (request-uri (&key request)
    (:documentation "Returns the request URI."))
  (content-type (&key response)
    (:documentation "Returns the currenct set content-type for the response."))
  (set-cookie (name &key value domain path expires http-only secure response)
    (:documentation "Sets a cookie to be sent on the response."))
  (set-content-type (content-type &key response)
    (:documentation "Sets the Content-Type HTTP header."))
  (set-default-content-type (content-type)
    (:documentation "Sets the default Content-Type HTTP header used on new responses."))
  (set-return-code (return-code &key response)
    (:documentation "Sets the HTTP return code."))
  (set-header (name value &key response)
    (:documentation "Sets an HTTP header for the response."))
  (set-response-content (content &key response)
    (:documentation "Set the main HTTP response body."))
  (redirect (new-address &key response)
    (:documentation "Sets the HTTP Redirect header."))
  (uploaded-file (post-parameter &key request)
    (:documentation "Returns four values: 1) pathname of the temporary file that was uploaded 2) a string of the original filename 3) the supplied mime-type of the file 4) the file size in bytes."))
  (serve-file (pathname &key content-type response)
    (:documentation "Send a file from disk. If content-type is NIL, it is attempted to determine it from the file."))
  (set-handler-function (handler-fun)
    (:documentation "Sets the handler function that the request and response instances are dispatched to. Should default to RADIANCE:HANDLER ."))
  (with-gets (vars &body body)
    (:type :macro)
    (:documentation "Same as WITH-SLOTS but for GET variables."))
  (with-posts (vars &body body)
    (:type :macro)
    (:documentation "Same as WITH-SLOTS but for POST variables."))
  (with-posts-or-gets (vars &body body)
    (:type :macro)
    (:documentation "Same as WITH-SLOTS but for POST or GET variables."))
  (with-headers (vars &body body)
    (:type :macro)
    (:documentation "Same as WITH-SLOTS but for headers."))
  (with-cookies (vars &body body)
    (:type :macro)
    (:documentation "Same as WITH-SLOTS but for cookies. Note that SETF-ing a variable bound by this will use the default values of SERVER:SET-COOKIE.")))

(define-interface dispatcher
  (dispatch (request)
    (:documentation "Dispatch a new webserver call."))
  (register (hook module uri)
    (:documentation "Register a hook to dispatch to on the given URI."))
  (unregister (uri)
    (:documentation "Free a given URI."))
  (effective-trigger (uri)
    (:documentation "Return the trigger and URI that would be called on the given request URI."))
  (dispatch-default (request)
    (:documentation "The standard method to invoke when no specific handler has been found.")))

(define-interface user
  (class ()
    (:documentation "User base class")
    (:type :class))
  (current (&key default authenticate)
    (:documentation "Returns the currently logged in user or if provided a default value. If authenticate is non-NIL, it will issue an AUTH:AUTHENTICATE call first."))
  (get (username)
    (:documentation "Returns the user object of an existing user or creates a new hull instance."))
  (field (user field &key value)
    (:documentation "Set or get a user data field.")
    (:type :accessor) (:class class))
  (save (&key user)
    (:documentation "Save the user to the database. USER defaults to USER:CURRENT."))
  (saved-p (&key user)
    (:documentation "Returns T if the user is not a hull instance, otherwise NIL. USER defaults to USER:CURRENT."))
  (check (branch &key user)
    (:documentation "Checks if the user has access to that permissions branch. USER defaults to USER:CURRENT."))
  (grant (branch &key user)
    (:documentation "Give permission to a certain branch. USER defaults to USER:CURRENT."))
  (prohibit (branch &key user)
    (:documentation "Reclaim/Prohibit permission to a certain branch."))
  (action (action &key user public)
    (:documentation "Record an action for the user. If PUBLIC is NIL, the action should not be visible to anyone else. USER defaults to USER:CURRENT."))
  (actions (n &key user public oldest-first)
    (:documentation "Returns a list of N cons cells, with the car being the action and the cdr being the time of the action. USER defaults to USER:CURRENT.")))

(define-interface auth
  (authenticate ()
    (:documentation "Authenticate the current user using whatever method applicable. Returns the user object."))
  (authenticated-p (&key session)
    (:documentation "Returns T if the current user is using an authenticated session, NIL otherwise."))
  (page-login (&key redirect)
    (:documentation "Returns an URL to the login page of the auth system. If redirect is provided, the user will be taken to that page afterwards."))
  (page-logout (&key redirect)
    (:documentation "Returns an URL to the logout page of the auth system. If redirect is provided, the user will be taken to that page afterwards."))
  (page-register (&key redirect)
    (:documentation "Returns an URL to the registration page of the auth system. If redirect is provided, the user will be taken to that page afterwards."))
  (page-options (&key target)
    (:documentation "Either displays a full options page or inserts all necessary things into the target if provided.")))

(define-interface session
  (class ()
    (:documentation "Sessions base class")
    (:type :class))
  (get (uuid)
    (:documentation "Returns the session for the given UUID or NIL if no session is found."))
  (get-all ()
    (:documentation "Return all sessions."))
  (start (username)
    (:documentation "Creates a new session object for the given user."))
  (start (user)
    (:documentation "Creates a new session object for the given user."))
  (start-temp ()
    (:documentation "Creates a temporary session without a bound user."))
  (uuid (session)
    (:documentation "Returns the uuid for this session."))
  (user (session)
    (:documentation "Returns the user associated with this session."))
  (field (session field &key value)
    (:documentation "Set or get a session data field.")
    (:type :accessor) (:class class))
  (end (session)
    (:documentation "Finalizes the session object and in effect logs the user out."))
  (active-p (session)
    (:documentation "Returns T if the session is still active, otherwise NIL."))
  (temp-p (session)
    (:documentation "Returns T if the session is only temporary, otherwise NIL.")))

(define-interface profile
  (field (user name &key value default)
    (:documentation "Retrieves or sets a custom user field. If the field is unset or does not exist, the default value is returned."))
  (avatar (user size)
    (:documentation "Returns an URL to the avatar of the user, in the closest available size to the one requested."))
  (name (user)
    (:documentation "Returns the displayable name of the user."))
  (page-settings (user)
    (:documentation "Returns the URL to the settings page for the user."))
  (page-user (user)
    (:documentation "Returns the URL to the user's profile page."))
  (define-panel (name category (&key lquery access-branch menu-icon menu-tooltip) &body body)
    (:type :MACRO)))

(define-interface (database db)
  (connect (dbname)
    (:documentation "Connects to the database given the information in the arguments."))
  (disconnect ()
    (:documentation "Disconnects the database"))
  (connected-p ()
    (:documentation "Returns T if the database is connected, otherwise NIL."))
  (collections ()
    (:documentation "Returns a list of all existing collections."))
  (create (collection fields &key indices (if-exists :ignore))
    (:documentation "Create a new collection with an optional list of indexed fields."))
  (empty (collection)
    (:documentation "Remove all records from this collection."))
  (drop (collection)
    (:documentation "Delete this collection entirely."))
  (select (collection query &key fields skip limit sort) 
    (:documentation "Retrieve data from the collection. Query should be constructed with the query macro."))
  (iterate (collection query function &key fields skip limit sort) 
    (:documentation "Iterate over data in the collection. Query should be constructed with the query macro. Might be faster than SELECT."))
  (insert (collection data) 
    (:documentation "Insert the data into the collection. Data is a list of alists."))
  (remove (collection query &key (skip 0) (limit 0) sort) 
    (:documentation "Delete data from the collection. Query should be constructed with the query macro."))
  (update (collection query data &key skip limit sort replace) 
    (:documentation "Update data in the collection. Query should be constructed with the query macro and data is a list of alists."))
  (apropos (collection)
    (:documentation "Returns a list of all available fields and their type or NIL if any field is possible."))
  (query (&rest statements)
    (:documentation "Query macro to construct database queries. Usable functions include: := :<= :>= :< :> :in :matches :and :or :not")
    (:type :MACRO)))

(define-interface (data-model dm)
  (class ()
    (:documentation "Data-model base class.")
    (:type :class))
  (id (model)
    (:documentation "Returns the UID of the model."))
  (field (model field &key value)
    (:documentation "Returns the value of a field. Is setf-able.")
    (:type :accessor) (:class class)) 
  (get (collection query &key (skip 0) (limit 0) sort)
    (:documentation "Returns a list of model instances built from the query result."))
  (get-one (collection query &key (skip 0) sort)
    (:documentation "Returns the model instance of the first query result."))
  (hull (collection)
    (:documentation "Returns an empty model hull that can be used to insert data."))
  (hull-p (model)
    (:documentation "Returns T if the model is a hull, otherwise NIL."))
  (save (model)
    (:documentation "Updates the model in the database or throws an error if it does not exist."))
  (delete (model)
    (:documentation "Deletes the model from the database."))
  (insert (model &key clone)
    (:documentation "Inserts the model into the database.")))

(defmacro with-fields ((&rest field-spec) model &body body)
  "Lets you access fields directly by name. This is similar to with-accessors.
Each field-spec can either be a symbol depicting the variable and field to bind or a list of a symbol
and a string, denoting variable name and field name respectively."
  (let ((vargens (gensym "MODEL")))
    `(let ((,vargens ,model))
       (symbol-macrolet
           ,(loop for field in field-spec 
               for varname = (if (listp field) (first field) field)
               for fieldname = (if (listp field) (second field) (string-downcase (symbol-name field)))
               collect `(,varname (data-model::i-field data-model::*implementation* ,vargens ,fieldname)))
         ,@body))))

(defmacro with-model (model-spec (collection query &key (skip 0) sort save) &body body)
  "Allows easy access to a single model.
Model-spec can be either just the model variable's name, or a list starting with the model's name,
followed by field specifiers like in with-fields. Query should either be a database query or NIL if
a hull is required. If save is non-NIL, a model-save is executed after the body. The return value
of this is always the last statement in the body, even if save is non-NIL."
  (let* ((returngens (gensym "RETURN"))
         (modelname (if (listp model-spec) (car model-spec) model-spec))
         (modelfields (if (listp model-spec) (cdr model-spec) NIL)))
    (if save (setf body `((let ((,returngens (progn ,@body))) (data-model:save ,modelname) ,returngens))))
    (if modelfields (setf body `((with-fields ,modelfields ,modelname ,@body))))
    `(let ((,modelname ,(if query
                            `(data-model:get-one ,collection ,query :skip ,skip :sort ,sort)
                            `(data-model:hull ,collection))))
       (when ,modelname
         ,@body))))

(define-interface admin
  (define-panel (name category (&key lquery access-branch menu-icon menu-tooltip) &body body)
      (:type :MACRO)))

(define-interface parser
  (parse (text)
    (:documentation "Parses the given text into HTML format, ready to be outputted.")))
