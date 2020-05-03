#|
 This file is a part of Radiance
 (c) 2016 Shirakumo http://tymoon.eu (shinmera@tymoon.eu)
 Author: Nicolas Hafner <shinmera@tymoon.eu>
|#

(in-package #:org.shirakumo.radiance.core)

;; api.lisp
(docs:define-docs
  (variable *api-formats*
    "Map from names to api format translator functions.

See API-FORMAT")

  (variable *default-api-format*
    "The API format to use if no specific one is requested.

See *API-FORMATS*")

  (function api-format
    "Accessor to the api formats.

The api format function should accept a single argument, the
object to translate, and should configure the response to
output the correct data, or in the very least return an
acceptable value for the response's data.

The name must be a string designator.

See *API-FORMATS*
See REMOVE-API-FORMAT
See LIST-API-FORMATS
See DEFINE-API-FORMAT
See API-OUTPUT")

  (function remove-api-format
    "Removes the named api format if it exists.

The name must be a string designator.

See *API-FORMATS*
See API-FORMAT")

  (function define-api-format
    "Define a new api format.

You should configure the *RESPONSE* to output the properly
serialised content of the argument you receive.

Note that the structures must be recursively serialised and
you may not error when encountering any of the types listed
as permitted in API-OUTPUT.

See API-FORMAT
See API-OUTPUT")

  (function api-output
    "Emits the given data as a response.

This function should be called for any and all return data
from an api endpoint. It ensures the data proper structure,
failure handling, and data translation to the desired output
format.

The proper structure for an API response from an endpoint
should be an object/table/map with the following fields:

  status   --- The HTTP status code.
  message  --- A supplied human-readable message that describes
               the success or failure.
  data     --- The data payload of the api output.

The types of objects that can be serialised via this function
are restricted to the following set:

- REAL
- (EQL NIL)
- (EQL T)
- STRING
- LIST (proper ones)
- VECTOR
- HASH-TABLE

An api format may support additional types, but is not
required to. Thus, in order to be conforming, the data you
pass to this function must not reference any values that
have a type outside of this set.

See API-FORMAT")
  
  (function api-serialize
    "Cause the object to be serialised into a more favourable structure.

This function should be used by api-format providers to
transform objects that cannot be directly emitted into
a structure that can.

If the object is not serializable, an error of type
API-UNSERIALIZABLE-OBJECT is signalled.")

  (variable *api-endpoints*
    "A map from names to api-endpoints.

See API-ENDPOINT")

  (function api-endpoint
    "Accessor to the api-endpoint instances.

See API-ENDPOINT
See REMOVE-API-ENDPOINT
See LIST-API-ENDPOINT")

  (function remove-api-endpoint
    "Removes the given api page again if it exists.

See API-ENDPOINT")

  (function list-api-endpoints
    "Lists all known api page instances.

See API-ENDPOINT")

  (function ensure-api-endpoint
    "Ensures to return the api-endpoint instance for the name.

Accepted types are:
  STRING    --- Looks up the api page by its name
  SYMBOL    --- Coerces to string and tries again
  API-ENDPOINT  --- Returns the argument

If the page cannot be looked up, an error is signalled.

See API-ENDPOINT")

  (type api-endpoint
    "Container for an api endpoint.

See NAME
See HANDLER
See ARGSLIST
See REQUEST-HANDLER")

  (function name
    "Accesses the name of the object.

May be a symbol or a string depending on the object.")

  (function handler
    "Accesses the handler function of the api-endpoint.

The handler-function must have the same lambda-list as
the api-endpoint's ARGSLIST. It must call API-OUTPUT
at some point during its evaluation to emit data.

See API-ENDPOINT")

  (function argslist
    "Accesses the lambda-list of the api-endpoint's handler.

This describes the public interface's arguments.
Arguments are usually received as GET or POST
parameters of the same name as the argument symbol and it
thus only makes sense for the lambda-list to contain 
required and optional arguments.

See API-ENDPOINT")

  (function request-handler
    "Accesses the function to handle a direct request object and transform it into a proper api call.

This function is usually automatically generated.
It should read out the parameters from the request object
and turn them into arguments for a call to the api-endpoint's 
handler function. The function only takes a single argument
namely the request object to handle.

See HANDLER
See API-ENDPOINT")

  (function make-request-handler-function
    "Constructs a standard request handler function for api-endpoints.

This constructs a lambda expression that extracts the
values from the request's post/get variables and turns
them into arguments for a call to the given function.

See REQUEST-HANDLER")

  (function call-api-request
    "Calls the given api-endpoint with the given request.

This rebinds *REQUEST*.

See REQUEST-HANDLER
See ENSURE-API-ENDPOINT")

  (function call-api
    "Calls the given api-endpoint directly with the supplied arguments.

See HANDLER
See ENSURE-API-ENDPOINT")

  (function define-api
    "Defines a new api endpoint.

NAME     --- The name of the endpoint. This also designates
             where the endpoint will be reachable. By default
             any endpoint can be reached on a /api/[name] path.
ARGS     --- The lambda-list for the arguments to the api endpoint.
             Usually, unless a specific request-handler is used,
             only required and optional arguments can be used.
             Their names correspond to the names that will be used
             to read out their values from the POST/GET variables of
             a request.
OPTIONS  --- A list of options that modify the api endpoint in some
             way.
BODY     --- A number of body forms that compose the actual
             functionality of the api endpoint. Should call API-OUTPUT
             at some point.

A standard uri-dispatcher called API is responsible for calling the
api pages when the path /api/ is requested.

Api definitions are transformed by options of the type API.

See API-OUTPUT
See API-ENDPOINT
See HANDLER
See REQUEST-HANDLER
See CALL-API
See CALL-API-REQUEST
See EXPAND-OPTIONS")

  (function api-error
    "Shorthand for (ERROR 'API-ERROR :MESSAGE (FORMAT NIL ..))

See API-ERROR")

  (page api
    "Standard page to handle and dispatch to API endpoints.

Respects the \"browser\" post/get property, which if set to
the string \"true\", causes a redirect to the referer on
an api-error rather than displaying a machine-readable
error output.

See API-ENDPOINT
See CALL-API-REQUEST"))

;; conditions.lisp
(docs:define-docs
  (type radiance-condition
    "Base condition class for all conditions related to radiance.

Contains a MESSAGE slot that may hold a string that
explains the reason around the condition in human-
readable format.

See MESSAGE")

  (function message
    "Accessor to the message of the condition.

See RADIANCE-CONDITION")

  (type radiance-error
    "Base condition class for all errors related to radiance.

See RADIANCE-CONDITION")

  (type radiance-warning
    "Base condition class for all warnings related to radiance.

See RADIANCE-CONDITION")

  (type definition-for-shared-package
    "Warning signalled when a definition is made on a symbol that may lead to unintentional clashes.

This warning gives you an indication for when you might be
accidentally exposing your definition to overrides by other
systems.

See RADIANCE-WARNING")

  (type system-has-no-version
    "Error signalled when an ASDF system does not store a version string.

Without a version string, Radiance is incapable of tracking what the
current version of a system is and is thus unable to automatically
migrate it.

This error should be continuable.

See MIGRATE
See RADIANCE-ERROR")

  (type backwards-migration-not-allowed
    "Error signalled when a migration from a later version to an earlier version is attempted.

See MIGRATE
See RADIANCE-ERROR")

  (type environment-not-set
    "Error signalled when an action was performed that requires an initialised environment, but no environment has been configured yet.

See ENVIRONMENT
See RADIANCE-ERROR")

  (type internal-error
    "Base condition class for internal errors that are unrecoverable.

See RADIANCE-ERROR")

  (type request-error
    "Base condition class for errors related to requests.

Contains a REQUEST slot that holds the request that caused
the issue.

See RADIANCE-ERROR")

  (type request-empty
    "Error signalled when the reply body to the request was empty.

See REQUEST-ERROR")

  (type request-not-found
    "Error signalled when a resource was requested that does not exist.

See REQUEST-ERROR")

  (type request-denied
    "Error signalled when a resource was requested that cannot be displayed to the requestor as they do not have sufficient permission.

See REQUEST-ERROR")

  (type api-error
    "Base condition class for api related errors.

See REQUEST-ERROR")

  (type api-auth-error
    "Error signalled when a request to an api endpoint was unauthorised.

See API-ERROR")

  (type api-argument-missing
    "Error signalled when a required argument for the api endpoint was not supplied.

Contains an ARGUMENT slot that holds the name of the missing
argument.

See API-ERROR")

  (type api-argument-invalid
    "Error signalled when an argument was not of a permitted value.

Contains an ARGUMENT slot that holds the name of the invalid
argument.

See API-ERROR")

  (type api-call-not-found
    "Error signalled when an api endpoint was requested that does not exist.

See API-ERROR")

  (type api-unknown-format
    "Error signalled when an api format was required that is unknown.

Contains a FORMAT slot that holds the name of the requested
but missing api format.

See API-FORMAT
See API-ERROR")

  (type api-unserializable-object
    "Error signalled when an object was attempted to be serialized that cannot be.

Contains an OBJECT slot that holds the object that could not
be serialized.

See API-ERROR")

  (type interface-condition
    "Base condition class for interface related problems.

Contains an INTERFACE slot that holds an interface designator
to the interface that encountered a problem.

See RADIANCE-CONDITION")

  (type interface-implementation-not-set
    "Error signalled when an implementation for an interface was attempted to be loaded, but no corresponding implementation is configured.

See INTERFACE-CONDITION
See RADIANCE-ERROR")

  (type interface-implementation-not-present
    "Error signalled when an implementation for an interface was required for an action to be performed, but no implementation had been loaded.

See INTERFACE-CONDITION
See RADIANCE-ERROR")

  (type unparsable-uri-string
    "Error signalled when a string was attempted to be turned into an URI object that could not be parsed according to the URI rules.

Contains a STRING slot that holds the string that was attempted
to be parsed.

See URI
See RADIANCE-ERROR")

  (type no-such-post-parameter
    "Error signalled when a post parameter was requested that does not exist.

Contains a PARAMETER slot that holds the name of the requested
slot.

See REQUEST-ERROR")

  (type post-parameter-not-a-file
    "Error signalled when a post parameter was attempted to be interpreted as a file, while it is not actually one.

Contains a PARAMETER slot that holds the name of the requested
slot.

See REQUEST-ERROR")

  (type file-to-serve-does-not-exist
    "Error signalled when a file is attempted to be served that does not exist on the filesystem.

Contains a FILE slot that holds the pathname of the requested
file.

See REQUEST-ERROR"))

;; defaults.lisp
(docs:define-docs
  (option (page :hook)
    "Adds a hook that is automatically triggered when the page is called.

Unless otherwise specified, the hook is named the same as the page.")

  (option (page :uri-groups)
    "Allows capturing the regex groups in the URI's path as variables.

See CL-PPCRE:REGISTER-GROUPS-BIND")

  (function transform-access-body
    "Transforms the body to be protected from access by the permission branch.")

  (option (page :access)
    #1="Ensures that the page is only accessible if the user requesting it has the appropriate permission branch.")
  
  (option (api :access)
    #1#)

  (option (admin:panel :access)
    #1#)

  (option (profile:panel :access)
    #1#)

  (api-endpoint ||
    "Fallback api endpoint that signals an API-CALL-NOT-FOUND error.")

  (page favicon
    "Standard page for the favicon.ico root image.")

  (page robots
    "Standard page for the robots.txt root file.")

  (page static
    "Standard delegate for static files.

The address must be of the form /static/module/path
where the path is translated to one relative to the
module's static resource directory.

See STATIC-FILE")

  (variable *domain-internalizers*
    "A list of functions that potentially capture and remove known domain parts from the URI.")

  (function add-domain
    "Adds a new top-level domain to the list of recognised domains.

Adds the name to (MCONFIG :RADIANCE :SERVER :DOMAINS)

The top-level domain as thought of by radiance is any
domain that does not contain any subdomains in its name.
For example, if your website is hosted on example.com
then that would be a top-level domain. Or, if your domain
is some.deep.thing.network.internal then that too would be
a top-level domain.

Radiance needs to know this in order to be able to distinguish
when subdomains for its own purposes start and end since it
is not generally predictable for an arbitrary domain name.

See MCONFIG
See REMOVE-DOMAIN")

  (function remove-domain
    "Removes a known top-level domain.

See ADD-DOMAIN")

  (function compile-domain-internalizers
    "Compiles the known top-level domain to optimised recogniser functions.

If the domain does not have an internalizer, it will not
be properly stripped from the request.

See ADD-DOMAIN
See REMOVE-DOMAIN
See *DOMAIN-INTERNALIZERS*")

  (route (domain :mapping)
    "Ensures that top-level domains are stripped from the uri.

Depends on *DOMAIN-INTERNALIZERS*")

  (route (domain :reversal)
    "Ensures that the appropriate top-level domain is added to the uri.")

  (route (virtual-module :mapping)
    "Allows using a path of /!/module/path to simulate a call to a subdomain.

This translates the path by prepending the module to the subdomains
of the uri and resetting the path to the sub-path. The module is also
stored in the request's data field called VIRTUAL-MODULE.")

  (route (virtual-module :reversal)
    "Properly reverses the uri if the current request context was made under a virtual-module.

Uses the request's data field called VIRTUAL-MODULE to know
where we came from."))

;; dispatch.lisp
(docs:define-docs
  (type uri-dispatcher
    "Container object for a link between a uri and a page function.

A uri-dispatcher should be called from a request and is then
responsible for generating the content of the response.

See NAME
See DISPATCH-FUNCTION
See PRIORITY
See DISPATCH
See URI-DISPATCHER")

  (function dispatch-function
    "Accessor to the function that performs the actual response building of the dispatch.

The function should not take any arguments.
The body should either set the data of the *RESPONSE*
object or return suitable data for it.

See URI-DISPATCHER")

  (function priority
    "Accessor to the priority, which may be NIL or an INTEGER.

See URI-DISPATCHER
See DISPATCH
See ROUTE")

  (variable *uri-registry*
    "Map from names to uri-dispatcher instances.

See URI-DISPATCHER")

  (variable *uri-priority*
    "An automatically generated vector containing the properly ordered sequence of uri-dispatchers.

See URI-DISPATCHER
See REBUILD-URI-PRIORITY
See DISPATCH")

  (variable *uri-fallback*
    "The fallback function to call when no matching uri-dispatcher could be found.

The function should not take any arguments and
otherwise act just like a uri-dispatcher dispatch-
function.

See DISPATCH")

  (function uri-dispatcher
    "Accessor to the registered uri-dispatcher instances.

Setting this automatically invokes a rebuild of the
*uri-priority* vector.

See *URI-REGISTRY*
See URI-DISPATCHER
See REMOVE-URI-DISPATCHER
See LIST-URI-DISPATCHERS
See REBUILD-URI-PRIORITY")

  (function remove-uri-dispatcher
    "Removes the named uri-dispatcher, if any.

See *URI-REGISTRY*
See URI-DISPATCHER")

  (function list-uri-dispatchers
    "Returns a list of all uri-dispatchers that are registered.

See *URI-REGISTRY*
See URI-DISPATCHER")

  (function uri-dispatcher>
    "Returns true if the uri-dispatcher A has a higher priority than B.

If neither A nor B have a priority set, the
comparison is the same as URI>. If only A has a
priority set, then T is returned. If only B has a
priority set, then NIL is returned. Otherwise T
is returned if the priority of A is greater or equal
to that of B.

See URI>")

  (function rebuild-uri-priority
    "Rebuilds the optimised and sorted vector of uri-dispatchers.

Sorting is done according to URI-DISPATCHER>.
Only uri-dispatchers that are registered are considered.

See URI-DISPATCHER>
See *URI-PRIORITY*
See *URI-REGISTRY*")

  (function define-uri-dispatcher
    "Defines a new uri-dispatcher.

The body forms will be evaluated if DISPATCH is called
with a URI object that matches the URI given in the 
definition and if no other uri-dispatcher precedes this
one in their priority.

See URI-DISPATCHER
See URI-DISPATCHER>
See DISPATCH-FUNCTION
See DISPATCH")

  (function dispatch
    "Calls the appropriate uri-dispatcher that is set up to handle the given uri.

Usually, only a single uri-dispatcher will be called, if any.
In order for a dispatcher to be called, the uri must match
the dispatcher's by URI-MATCHES. In the case where two
uri-dispatchers have a matching uri, then the one with the
higher priority will be executed. This is achived by simply
following the order present in the *URI-PRIORITY* vector.

A uri-dispatcher that has been dispatched may call the
ABORT-HANDLING restart, in which case it is considered as 
not having matched, and the dispatching continues.

If no matching uri-dispatcher is available, the function
in *URI-FALLBACK* will be called instead.

See URI-MATCHES
See URI-DISPATCHER>
See DISPATCH-FUNCTION
See ABORT-HANDLING
See *URI-PRIORITY*
See *URI-FALLBACK*")

  (function abort-handling
    "Aborts the current handling and continues dispatch.

This is a wrapper function that simply invokes the
ABORT-HANDLING restart.

See DISPATCH"))

;; documentable.lisp
(docs:define-docs
  (type documentable
    "Superclass for all classes that can be documented.

Use DOCUMENTATION to access the docstring.

See CL:DOCUMENTATION
See DEFINE-DOCUMENTABLE")

  (function define-documentable
    "Defines a new documentable class.

If the class-option :FIND-FUNCTION is given, shortcut
functions for DOCUMENTATION are additionally created
that allow using just the name of an instance to
access the docstring. The find-function is called
with a single argument, the name of the instance to find.

See DOCUMENTABLE"))

;; environment.lisp
(docs:define-docs
  (variable *environment*
    "Holds the currently configured environment name, if any.

See ENVIRONMENT")

  (hook environment-change
    "This hook is triggered after the environment has been changed.

At this point the environment is already set up and the
configuration has been reloaded or purged as necessary.")

  (function environment-directory
    "Returns the base directory for the given environment and file kind.

If the ENVIRONMENT parameter is T, it is treated as if it were
the value of (ENVIRONMENT).

The administrator is encouraged to provide specialised methods
on this generic function in order to redefine the base directories
used for the environment's files.

The following KINDs are currently recognised internally, though
the user may supply further kinds.

  :CONFIGURATION  --- The base for configuration files. The
                      MCONFIG/CONFIG functions access files in
                      this base directory.
  :CACHE          --- The base for temporary cache files that
                      may be cleared out without consequence.
  :DATA           --- The base for data files. Modules may use
                      this directory to store data files for
                      runtime caches, user data, etc.
  :TEMPLATE       --- The base for template files. Administrators
                      may use this directory to provide overrides
                      for a module's template files.
  :STATIC         --- The base for static files. Administrators
                      may use this directory to provide overrides
                      for a module's static files.

By default the base paths are determined as follows:

  :CONFIGURATION
1. A root is discovered as one of the following alternatives:
1.1. The XDG_CONFIG_HOME environment variable is consulted.
1.2. On Windows the AppData environment variable is consulted.
1.3. On Windows the directory ~/Application Data/ is used.
1.4. The directory ~/.config/ is used.
2. A subdirectory called \"radiance\" is added to the root.
3. A subdirectory with the environment name is added to the root.

  :CACHE
1. A root is discovered as one of the following alternatives:
1.1 The XDG_CACHE_HOME environment variable is consulted.
1.2 On Windows the TEMP environment variable is consulted.
1.3 On Windows the directory ~/Local Settings/Temp/ is used.
1.4 The directory ~/.cache/ is used.
2. A subdirectory called \"radiance\" is added to the root.
3. A subdirectory with the environment name is added to the root.

  :DATA/:TEMPLATE/:STATIC
1. A root is discovered as one of the following alternatives:
1.1 The XDG_DATA_HOME environment variable is consulted.
1.2 On Windows the LocalAppData environment variable is consulted.
1.3 On Windows the directory ~/Local Settings/ is used.
1.4 The directory ~/.local/share/ is used.
2. A subdirectory called \"radiance\" is added to the root.
3. A subdirectory with the environment name is added to the root.
4. A subdirectory with the name of the type (data/template/static) 
   is added to the root.

For instance, the environment directory on a clean Linux system
for the \"default\" environment and :configuration kind would be:

  ~/.config/radiance/default/

The same on a recent (Vista+) Windows system would be:

  ~/AppData/Roaming/radiance/default/

And on an older (XP-) Windows system it would be:

  ~/Application Data/radiance/default/")

  (function environment-module-directory
    "Returns the base directory for the given module and file kind.

The administrator is allowed to supply custom methods to this
function that specialise on specific modules to tightly control
the behaviour.

If the environment is unset, this function will signal an error.

By default this simply retrieves the environment root directory
for the requested file kind and adds a subdirectory named after
the downcased module name.

For instance, the module directory for the radiance-core
module in the default environment for the :configuration kind
on Linux would be:

  ~/.config/radiance/default/radiance-core/

See ENVIRONMENT-DIRECTORY
See ENVIRONMENT
See CHECK-ENVIRONMENT")

  (function environment-module-pathname
    "Returns a path within the module's file directory for the given kind.

This simply performs a merge-pathnames call on the given pathname
and the corresponding ENVIRONMENT-MODULE-DIRECTORY.

See ENVIRONMENT-MODULE-DIRECTORY")

  (function environment
    "Accessor to the current environment.

The environment decides the namespace for the configuration
and data files of Radiance and all modules that use its system.

Note that changing the environment after one has already
been loaded and modules have been loaded with it, is currently
not supported and will lead to strange behaviour.

See *ENVIRONMENT*
See CHECK-ENVIRONMENT
See MCONFIG-PATHNAME")

  (function check-environment
    "Checks whether the environment is properly configured.

If no environment is present, an error of type
ENVIRONMENT-NOT-SET is signalled. Two restarts will
be present at the time:
  CONTINUE         --- Sets the environment to \"default\"
  SET-ENVIRONMENT  --- Sets the environment to the one
                       passed in the argument.

See ENVIRONMENT")

  (function reload-environment
    "Reloads the current environment from disk.

Note that configuration files are reloaded lazily, with the
exception of the radiance-core configuration.

See CHECK-ENVIRONMENT
See ENVIRONMENT")

  (function sync-environment
    "Forces the writing of all configuration files.

This will cause the configuration file for every loaded module to
be written to disk. Note that this will /not/ necessarily first
try to load the files from disk, so changes to the files may be
overwritten and lost by this operation.

Typically configuration files are automatically synced to disc
on write, so this function should not be necessary most of the
time.

See MODULARIZE:LIST-MODULES
See MCONFIG-STORAGE")

  (function mconfig-pathname
    "Returns the proper pathname to the module according to the current environment.

The path is constructed according to:
  :NAME     Set to the downcased module-name.
  :TYPE     Set to conf.TYPE with type downcased.
  :DEFAULTS ENVIRONMENT-MODULE-DIRECTORY for the
            given module and the :CONFIGURATION kind.

An environment must have been configured prior to calling
this function.

See ENVIRONMENT-MODULE-DIRECTORY
See ENVIRONMENT")

  (function mconfig-storage
    "Returns the storage object for the given module.

The storage object is cached and will only be loaded
if it has not previously been loaded. This means that
unless explicit cache purging occurs, changes to the
underlying configuration file will be lost on
subsequent writes.

The object is cached in the module-storage slot :CONFIG

See MODULARIZE:MODULE-STORAGE
See MCONFIG-PATHNAME
See UBIQUITOUS:RESTORE
See UBIQUITOUS:OFFLOAD")

  (function mconfig
    "Accesses a configuration variable for the given module's storage.

See UBIQUITOUS:VALUE
See MCONFIG-STORAGE")

  (function defaulted-mconfig
    "Sets the configuration variable to the given default if it has not been set previously and returns the value.

See UBIQUITOUS:DEFAULTED-VALUE
See MCONFIG-STORAGE")

  (function remmconfig
    "Removes the configuration place from the given module's configuration.

See UBIQUITOUS:REMVALUE
See MCONFIG-STORAGE")

  (function config
    "Shorthand to access the current module's configuration.

This has to be a macro so that the current package can be
captured.

See MCONFIG")

  (function defaulted-config
    "Shorthand to set/retrieve a defaulted value from the module's configuration.

This has to be a macro so that the current package can be
captured.

See DEFAULTED-MCONFIG")

  (function remconfig
    "Shorthand to remove a place from the module's configuration

This has to be a macro so that the current package can be
captured.

See REMMCONFIG")

  (function static-file
    "Returns the static file for the given base.

The base will usually be your local module. This function will return
the path returned by ENVIRONMENT-MODULE-PATHNAME for the given base,
:STATIC type, and namestring if the file exists, or otherwise merge
the namestring with the static/ subdirectory within your module's
source directory.

For instance, a module named FOO asking for the BAR static file while
the file is overridden would return a path like the following under
default setups:

  ~/.local/share/radiance/default/static/foo/bar

If this override file did not exist, the following path would be
returned instead assuming FOO's project root is at ~/Projects/:

  ~/Projects/foo/static/bar

See @STATIC")

  (function @static
    "Expands to a pathname to the static file in the current module.

This expands to a load-time-value form if the namestring is constant,
ensuring that no expensive lookups are done at runtime.

See STATIC-FILE")

  (function template-file
    "Returns the template file for the given base.

The base will usually be your local module. This function will return
the path returned by ENVIRONMENT-MODULE-PATHNAME for the given base,
:TEMPLATE type, and namestring if the file exists, or otherwise merge
the namestring with the template/ subdirectory within your module's
source directory.

For instance, a module named FOO asking for the BAR template file while
the file is overridden would return a path like the following under
default setups:

  ~/.local/share/radiance/default/template/foo/bar

If this override file did not exist, the following path would be
returned instead assuming FOO's project root is at ~/Projects/:

  ~/Projects/foo/template/bar

See @TEMPLATE")

  (function @template
    "Expands to a pathname to the template file in the current module.

This expands to a load-time-value form if the namestring is constant,
ensuring that no expensive lookups are done at runtime.

See TEMPLATE-FILE"))

;; handle.lisp
(docs:define-docs
  (variable *debugger*
    "Whether the debugger should be invoked when an error occurs during a request execution.

Can be one of the following values:
  :IF-SWANK-CONNECTED  -- Only invoke the debugger if there is an active swank connection.
  T                    -- Always invoke the debugger.
  NIL                  -- Never invoke the debugger.

See MAYBE-INVOKE-DEBUGGER
See HANDLE-CONDITION
See EXECUTE-REQUEST")

  (function maybe-invoke-debugger
    "Might call INVOKE-DEBUGGER depending on the value of *DEBUGGER*.

If the debugger is invoked, it is always invoked with a
CONTINUE restart surrounding it to avoid handling the
condition.

If the debugger is not invoked, or declines to handle the
condition via the CONTINUE restart, and the RESTART argument
is given, then the given restart is automatically invoked
with the remaining values.

See *DEBUGGER*")

  (function handle-condition
    "Responsible for handling a condition during a request execution.

Might invoke the debugger depending on *DEBUGGER*.
Otherwise invokes the SET-DATA restart with the
return-value of calling RENDER-ERROR-PAGE.

See *DEBUGGER*
See MAYBE-INVOKE-DEBUGGER
See EXECUTE-REQUEST
See RENDER-ERROR-PAGE")

  (function render-error-page
    "This function is responsible for rendering an appropriate response for the given condition.

A module is allowed to redefine this function to do as
it sees fit.

See HANDLE-CONDITION")

  (hook request
    "This hook is triggered whenever a request is executed.

It has two arguments: the request and the response object.

Note that at this point dispatch has not yet happened, but
the routing is already complete and the request's URI should
be in the internal representation.")

  (function execute-request
    "Directly executes the given request and response instances.

If an error occurs during the execution, 
HANDLE-CONDITION is called to handle it.
The REQUEST hook is called at the beginning of execution.
Then the request is dispatched on and the result converted
into data for the response if applicable.

See RESPONSE
See REQUEST
See DISPATCH
See HANDLE-CONDITION")

  (function ensure-request-hash-table
    "Attempts to coerce the given thing into a hash table suitable for the request object.")

  (function request
    "Handle a request to the given URI with the given parameters.

This creates an appropriate request object, translates the URI
object if necessary, and then calls EXECUTE-REQUEST to actually
perform the request proper.

See REQUEST
See ENSURE-REQUEST-HASH-TABLE
See REPRESENT-URI
See EXECUTE-REQUEST"))

;; init.lisp
(docs:define-docs
  (variable *startup-time*
    "Keeps the universal-time of when STARTUP was called.

See STARTUP
See SHUTDOWN
See UPTIME")

  (variable *running*
    "Whether Radiance is currently running or not.

See STARTUP
See SHUTDOWN
See STARTED-P")

  (hook server-start
    "This hook is triggered right after the server interface has been loaded.

At this point the server should be loaded to go, but not
necessarily yet running. This hook in itself causes the
server to start itself up.

Note that this is a sticky hook and will remain active
until SERVER-SHUTDOWN is triggered.

See SERVER-READY
See SERVER-SHUTDOWN")

  (hook server-shutdown
    "This hook is triggered after the server has been stopped and is shut down.

Note that this causes the SERVER-START hook to become
unstickied.

See SERVER-START")

  (hook server-ready
    "This hook is triggered after the server has been started up and is ready to receive requests.

Note that this is a sticky hook and will remain active
until SERVER-STOP is triggered.")

  (hook server-stop
    "This hook causes the server to stop and shut down.

Note that this causes the SERVER-READY hook to become
unstickied.

See SERVER-READY")

  (hook startup
    "This hook is triggered early in the startup sequence.

At this point the environment has been set and the logger
interface has been loaded, but nothing else has been
started yet.

Note that this is a sticky hook and will remain active
until SHUTDOWN is triggered.

See SHUTDOWN")

  (hook shutdown
    "This hook is triggered at the beginning of the shutdown sequence.

At this point nothing has been shut down yet.

Note that this causes the STARTUP hook to become
unstickied.

See STARTUP")

  (hook startup-done
    "This hook is triggered at the end of the startup sequence.

At this point the sequence is complete and the system
should be readily running.")

  (hook shutdown-done
    "This hook is triggered at the end of the shutdown sequence.

At this point the sequence is complete and the system
should be completely shut down.")

  (function startup
    "Starts up Radiance and prepares it for use.

If Radiance is already running, an error is signalled.
If ENVIRONMENT is not a string, an error is signalled.

The startup sequence proceeds as follows:
1. *STARTUP-TIME* is saved
2. The environment is changed
3. STARTUP is triggered
4. The implementation for the LOGGER interface is loaded
5. The implementation for the SERVER interface is loaded
6. SERVER-START is triggered
7. *RUNNING* is set to T
8. SERVER-READY is triggered
9. The systems in the (MCONFIG :RADIANCE :STARTUP)
   configuration are loaded in sequence
10. STARTUP-DONE is triggered

See SHUTDOWN
See STARTED-P")

  (function shutdown
    "Stops Radiance and cleans up all connections.

If Radiance is not already running, an error is signalled.

The shutdown sequence proceeds as follows:
1. SHUTDOWN is triggered
2. SERVER-STOP is triggered
3. *RUNNING* is set to NIL
4. SERVER-SHUTDOWN is triggered
5. *STARTUP-TIME* is set to NIL
6. SHUTDOWN-DONE is triggered

See STARTUP
See STARTED-P")

  (function uptime
    "Returns the amount of seconds that radiance has been started for, if at all.

See STARTUP
See SHUTDOWN
See *STARTUP-TIME*")

  (function started-p
    "Returns true if Radiance has been started up and is ready for use.

See STARTUP
See SHUTDOWN
See *RUNNING*"))

;; interface-components.lisp
(docs:define-docs
  )

;; interfaces.lisp
(docs:define-docs
  (type module
    "ASDF system subclass for modules.

See MODULARIZE:MODULE")

  (function module
    "Coerces the requested module.

See MODULARIZE:MODULE")

  (variable *old-dependency-def-fun*
    "Variable to keep the old definition of ASDF/PARSE-DEFSYSTEM::PARSE-DEPENDENCY-DEF.")

  (function future
    "Shorthand macro to help calling a function for a package or interface that has not yet been loaded at compile time.")

  (function find-implementation
    "Attempts to find a suitable implementation for the given interface.

This checks for the value of the configuration in
 MCONFIG :RADIANCE :INTERFACES interface-name-as-keyword

If SYSTEM is T, then the ASDF system object is returned
otherwise the implementation's system name.

If Quicklisp is available and the implementing system has
not yet been installed, it is installed automatically.
The system is not loaded, however.

If no implementation has been configured for the interface,
an INTERFACE-IMPLEMENTATION-NOT-SET error is signalled.

You may SETF this place to a suitable implementation for
the given interface. The implementation should be a
virtual module designator.")

  (function load-implementation
    "Attempts to silently load the implementation for the interface if necessary.

This function is called whenever an interface is requested
as a dependency in an ASDF system definition.

See FIND-IMPLEMENTATION")

  (function define-interface
    "Define a new module interface.

Unlike the native version of this macro, a hook-switch is
always defined that provides an IMPLEMENTED and UNIMPLEMENTED
hook, which will be called at the appropriate time. This
lets you react to when an implementation becomes active and
thus conditionally compile code without needing to incur a 
hard dependency.

See MODULARIZE-INTERFACES:DEFINE-INTERFACE
See DEFINE-IMPLEMENT-TRIGGER")

  (function define-implement-trigger
    "Defines a trigger that will cause the body to be compiled and run once the interface becomes implemented.

This is useful if you want to provide parts of a package
that depend on an interface being implemented, but do not
want to depend on the interface directly. Thus, using this
you can achieve optional/soft dependencies.

This depends on the IMPLEMENTED hook-switch present on every
Radiance interface. Since it is a hook-switch, a trigger like
this will be called automatically even if it is defined after
the interface has already been implemented.

Note that since the body is captured and quoted, and thus no
compilation will occur until the hook is triggered. This means
that you will potentially miss out on compilation errors or
information until later.

See DEFINE-INTERFACE"))

;; migration.lisp
(docs:define-docs
  (function encode-version
    "Encodes the given version into a version name as a keyword.

Each part of the version spec is concatenated as follows:
  VERSION-NAME    ::= (string|integer) (VERSION-STRING | VERSION-INTEGER)*
  VERSION-STRING  ::= '-' string
  VERSION-INTEGER ::= '.' integer

Essentially meaning that integer sub-versions are preceded by a
dot and string sub-versions by a dash.")
  
  (function parse-version
    "Parses the given string into a version spec.

The string should be of the following format:
  VERSION-NAME ::= VERSION-PART (('.' | '-') VERSION-PART)+
  VERSION-PART ::= integer | string

See ENSURE-PARSED-VERSION")
  
  (function ensure-parsed-version
    "Ensures that the given version is in the version spec format.

Returns a version spec:
  VERSION-SPEC ::= (VERSION-PART+)
  VERSION-PART ::= integer | string

See PARSE-VERSION")

  (function ensure-versions-comparable
    "Ensures the two versions are of the same length.

If one of the versions is shorter, it is extended by 0 elements
in its spec. Both of the versions are returned, with the specs
having equal length.

See ENSURE-PARSED-VERSION")

  (function version-part=
    "Returns T if the two version parts are considered equal.

The following table should cover all cases:
 ↓A B→ INTEGER  STRING
INTEGER    =      NIL
STRING    NIL   STRING=")

  (function version-part<
    "Returns T if the first version part is considered lower.

The following table should cover all the cases:
 ↓A B→ INTEGER  STRING
INTEGER    <       T
STRING    NIL   STRING<")

  (function version=
    "Returns T if the two version specs are considered equal.

First ensures both versions are of equal length, then calls
VERSION-PART= on each part of the versions. If they are all
VERSION-PART=, then T is returned and NIL otherwise.

A special exemption is made for NIL version specs. If both
versions are NIL, T is returned. Otherwise, if only one spec is
NIL, NIL is returned.

See ENSURE-VERSIONS-COMPARABLE
See VERSION-PART=")

  (function version<
    "Returns T if the first version is considered lower.

First ensures both versions are of equal length, then calls
VERSION-PART= on each part of the versions. If it returns NIL,
then VERSION-PART< is called. If this returns T, T is returned,
otherwise NIL is returned. If all parts are VERSION-PART=, NIL is
returned.

A special exemption is made for NIL version specs. If both
versions are NIL, NIL is returned. Otherwise, if the first spec
is NIL, T is returned. If the second spec is NIL, NIL is returned.

See ENSURE-VERSIONS-COMPARABLE
See VERSION-PART=
See VERSION-PART<")

  (function version<=
    "Returns T if the first version is considered lower.

First ensures both versions are of equal length, then calls
VERSION-PART= on each part of the versions. If it returns NIL,
then VERSION-PART< is called. If this returns T, T is returned,
otherwise NIL is returned. If all parts are VERSION-PART=, T is
returned.

A special exemption is made for NIL version specs. If both
versions are NIL, T is returned. Otherwise, if the first spec
is NIL, T is returned. If the second spec is NIL, NIL is returned.

See ENSURE-VERSIONS-COMPARABLE
See VERSION-PART=
See VERSION-PART<")

  (function version-region
    "Returns the versions in VERSIONS that are between START and END, inclusive.

For instance, the version list '(1 2 4 5 6) for the bounds 3 and 6
would return '(4 5 6).

The list of versions is assumed to be sorted by VERSION<.

See VERSION<=")

  (function version-bounds
    "Returns the versions in VERSIONS that are between START and END, inclusive, ensuring the sequence starts with START and ends with END, if provided.

For instance, the version list '(1 2 4 5 6) for the bounds 3 and 6
would return '(3 4 5 6).

The list of versions is assumed to be sorted by VERSION<.

See VERSION-REGION")
  
  (function last-known-system-version
    "Return the last known version of this system that had been migrated to.

Returns the version as an encoded keyword or NIL if the system
has not seen a migration previously. This version is automatically
adapted after MIGRATE-VERSIONS on the system completes
successfully.

See MIGRATE-VERSIONS")
  
  (function migrate-versions
    "Perform a migration of the system from the given source version to the given target version.

If a system or module requires manual intervention to upgrade
data or other parts in order to move between versions, the author
of the system should specialise a method on this function that
performs the requested upgrade step.

FROM and TO should be encoded versions in keyword form. FROM can
also be the NIL symbol, which is useful to migrate from previously
unknown versions to another.

Note that the version steps between migrate-version methods on the
same system should be contiguous. This means that if a system has
the concrete versions 1, 2, 3, and 4, then there should be methods
(if necessary) to upgrade from 1 to 2, from 2 to 3, from 3 to 4.
Migration steps with gaps, such as from 2 to 4, will not be
triggered by the system.

Also note that by default the list of concrete versions a system
offers are inferred from the methods defined on this function.
There is no need to further inform the system on available
concrete versions of a system.

A default method that performs no action is provided on this
function, as in the majority of cases no migration step is
required. As such, methods need only be added to this function if
an action is required.

Before the primary method is executed, ENSURE-DEPENDENCIES-READY
is called on the system and the source version. This should
ensure that dependant systems are on a required version for this
system to perform its own actions.

After the primary method has completed, the target version is
recorded as the last known concrete system version.

The user should NOT call this function. It is called by MIGRATE
as appropriate.

See DEFINE-VERSION-MIGRATION
See VERSIONS
See MIGRATE
See ENSURE-DEPENDENCIES-READY
See LAST-KNOWN-SYSTEM-VERSION")
  
  (function define-version-migration
    "A shorthand to define a version migration method for a system.

SYSTEM may be a string or symbol naming the ASDF system to define
a migration action for. FROM may be NIL, a symbol, or a string
naming the source version. TO may be a symbol or a string naming
the target version.

BODY should be a number of forms to be executed when the system
is moved from the source version to the target version.

See MIGRATE-VERSIONS")
  
  (function ready-dependency-for-migration
    "This function should ensure that the dependency conforms to the system's required state to migrate away from the given version.

By default this will invoke MIGRATE on the dependency with both
source and from versions being T.

The user should supply methods on this function to customise the
precise versions or features required to perform the migration of
the system properly.

See MIGRATE")
  
  (function ensure-dependencies-ready
    "This function should ensure that all dependencies of the given system are ready for the system to perform a migration away from the given version.

By default this will call READY-DEPENDENCY-FOR-MIGRATION on all
systems that are recorded in the system's defsystem-dependencies
and regular dependencies, AND are virtual-module systems.

The user should supply methods on this function in case it is
necessary to perform actions on other systems for the migration
to perform smoothly.

See READY-DEPENDENCY-FOR-MIGRATION
See ASDF:SYSTEM-DEFSYSTEM-DEPENDS-ON
See ASDF:SYSTEM-DEPENDS-ON")
  
  (function versions
    "Returns a list of concrete version designators that are known for the given system.

This list is deduplicated and sorted in such a way that lower
versions come earlier in the list.

By default this list is computed by inspecting the list of primary
methods on MIGRATE-VERSION, extracting the version specifiers, and
subsequently deduplicating and sorting the resulting list.

The user may supply methods on this function in case the
automatically computed list of versions is inadequate somehow.

See VERSION<
See MIGRATE
See MIGRATE-VERSIONS")
  
  (function migrate
    "Migrates the given system between the given versions.

Sometimes when versions change, a migration of runtime data is
required to ensure that the system still operates correctly on
existing data.

This function should ensure that this compatibility is achieved.

The system should be a designator for an ASDF system.

FROM may be one of the following:
  T       --- The system is migrated from its last known version.
  NIL     --- The system is migrated from a time before migrations.
  version --- The system is migrated from this specific version.

TO may be one of the following:
  T       --- The system is migrated to its current system version.
  version --- The system is migrated to this specific version.

When this function is called it determines the list of concrete
versions in the range of the specified FROM and TO versions.
It then calls MIGRATE-VERSION on the system and each pair of
versions in the computed range. For instance, if the list of
versions is (1 2 3 4), then MIGRATE-VERSION is called with the
pairs (1 2) (2 3) (3 4).

If the target version is T and the system has no recorded version,
an error of type SYSTEM-HAS-NO-VERSION is signalled. If the target
version is considered less than the source version, an error of
type BACKWARDS-MIGRATION-NOT-ALLOWED is signalled.

Two restarts are established during migration:
  ABORT         --- Aborts migration of the current system,
                    leaving the last known system version the
                    same.
  FORCE-VERSION --- Aborts the migration of the current system,
                    but forces the last known system version to
                    the requested target version.

See ENSURE-PARSED-VERSION
See VERSIONS
See MIGRATE-VERSION
See SYSTEM-HAS-NO-VERSION
See BACKWARDS-MIGRATION-NOT-ALLOWED"))

;; modules.lisp
(docs:define-docs
  (function module-domain
    "Returns the domain on which the module acts primarily.

Signals an error if the argument cannot be coerced to a module.
Signals an error of type INTERFACE-IMPLEMENTATION-NOT-PRESENT
if an interface was passed that does not have an implementation
yet.

If not explicit domain was configured, the module's name is
returned.

See MODULARIZE:MODULE")

  (function module-permissions
    "Returns the list of permissions that are known to exist for the module.

Signals an error if the argument cannot be coerced to a module.

See PERM
See MODULARIZE:MODULE")

  (function module-dependencies
    "Returns the list of dependencies for the given module.

Signals an error if the argument cannot be coerced to a module.
Signals an error if the module does not have an associated
virtual module.

See ASDF:SYSTEM-DEPENDS-ON
See MODULARIZE:MODULE
See MODULARIZE:VIRTUAL-MODULE")

  (function module-required-interfaces
    "Returns the list of interfaces that the module depends on.

Signals an error if the argument cannot be coerced to a module.

See MODULE-DEPENDENCIES
See MODULARIZE:MODULE")

  (function module-required-systems
    "Returns the list of systems that the module depends on.

Signals an error if the argument cannot be coerced to a module.

See MODULE-DEPENDENCIES
See MODULARIZE:MODULE")

  (function module-pages
    "Returns the list of URIs for pages that the module has registered.

Signals an error if the argument cannot be coerced to a module.

See MODULARIZE:MODULE")

  (function module-api-endpoints
    "Returns the list of names of the api endpoints that the module has registered.

Signals an error if the argument cannot be coerced to a module.

See MODULARIZE:MODULE")

  (function describe-module
    "Writes a human-readable description of the module to the stream.

This is useful for inspection and debugging, to see a quick
overview of what the module does or has. This function is
called by DESCRIBE if it is called on a module.

Signals an error if the argument cannot be coerced to a module.

See MODULE-DOMAIN
See MODULE-PERMISSIONS
See MODULE-REQUIRED-INTERFACES
See MODULE-REQUIRED-SYSTEMS
See MODULE-PAGES
See MODULE-API-ENDPOINTS
See MODULARIZE:MODULE")

  (variable *modules-directory*
    "Defines the path where modules should be created.")

  (function create-module
    "Creates a new stub module.

Creates the following files and directories:
  name/
  name/static
  name/template
  name/name.asd
  name/name.lisp

If Quicklisp is present, the local-projects are registered
and the project is loaded after the files have been created.

See *MODULES-DIRECTORY*")

  (function find-all-modules
    "Attempts to find all module systems in a directory.

This works as follows: it scans the directory tree for all
files with the type ASD. For each attempts of these, it
attempts to find an ASDF system with the file's name, and if
the system can be found and is of type VIRTUAL-MODULE, it is
returned.

See VIRTUAL-MODULE"))

;; options.lisp
(docs:define-docs
  (variable *options*
    "Map of names to option type tables.

Each option type table maps from names to OPTION instances.

See OPTION
See REMOVE-OPTION
See LIST-OPTIONS")

  (type option
    "Container class for an option expander.

See OPTION-TYPE
See NAME
See EXPANDER
See OPTION")

  (function option-type
    "Accesses the type of option that this option object belongs to.

See OPTION")

  (function expander
    "Accesses the actual expansion function of the object.

See OPTION")

  (function %option
    "Accessor wrapper for the purpose of the documentable.

See OPTION")

  (function option
    "Accessor to an option instance.

See OPTION
See *OPTIONS*
See REMOVE-OPTION
See LIST-OPTIONS")

  (function remove-option
    "Removes an option instance again, if it exists.

See OPTION
See *OPTIONS*
See LIST-OPTIONS")

  (function list-options
    "Lists all option instances that are registered.

See OPTION
See *OPTIONS*
See REMOVE-OPTION")

  (function define-option
    "Define a new option expander.

Option expanders are used to present an extensible mechanism
for adding functionality to various definition macros such
as DEFINE-PAGE, DEFINE-API, etc.

An option expander should return two values:
 1. The new list of body forms to use
 2. A single form to output before and outside of the
    definition.

The argument list must be congruent with the one defined by
the option type plus a final, optional value argument.
Usually the arglist will look like so:

  NAME BODY ARGS* [VALUE]

Where NAME is the definition name, BODY is the list of
body forms, and ARGS is any number of additional arguments
that the option type mandates.

See OPTION
See EXPAND-OPTIONS")

  (function expand-options
    "Expands all options of the given type.

Returns two values, the new body forms and the list of
forms to output before the actual definition.

An error is signalled if an unknown option is used.

See OPTION
See DEFINE-OPTION"))

;; page.lisp
(docs:define-docs
  (function define-page
    "Defines a new page that can be requested.

NAME     --- The name of the page. This is merely used to identify
             it uniquely.
URI      --- The actual URI on which the page will be found. This is
             an internal URI. The path is a regex.
OPTIONS  --- A list of options that modify the page definition in some
             way.
BODY     --- A number of body forms that compose the actual
             functionality of the page. Should set or return suitable
             data for the response.

Page definitions are transformed by options of the type PAGE.

See DEFINE-URI-DISPATCHER
See EXPAND-OPTIONS"))

;; request.lisp
(docs:define-docs
  (variable *request*
    "Bound to the current request object in the context of a request.

See EXECUTE-REQUEST
See REQUEST")

  (variable *response*
    "Bound to the current response object in the context of a request.

See EXECUTE-REQUEST
See RESPONSE")

  (variable *default-external-format*
    "The default external character encoding to use to send out the data.

See RESPONSE")

  (variable *default-content-type*
    "The default content-type to use to send out the data.

See RESPONSE")

  (function *request*
    "Returns the value of *REQUEST*

See *REQUEST*")

  (function *response*
    "Returns the value of *RESPONSE*

See *RESPONSE*")

  (type request
    "Container class to represent a request that was made against radiance.

See URI
See HTTP-METHOD
See BODY-STREAM
See HEADERS
See POST-DATA
See GET-DATA
See COOKIES
See DOMAIN
See REMOTE
See DATA
See ISSUE-TIME
See REQUEST
See USER-AGENT
See REFERER
See COOKIE
See GET-VAR
See POST-VAR
See POST/GET
See HEADER
See FILE
See REQUEST-RUN-TIME")

  (function uri
    "Accesses the URI that the request operates on.

Depending on the stage of the request, the URI may be either
internal or external.

See REQUEST")

  (function http-method
    "Accesses the HTTP method that was used to request the page.

Should be one of :GET :HEAD :POST :PUT :DELETE :TRACE :CONNECT

See REQUEST")

  (function body-stream
    "Accesses the request's binary input stream for the HTTP body.

This may return NIL, or a closed or empty stream if the HTTP request was
made with a content-type that is either application/x-www-form-urlencoded
or multipart/form-data. In that case, the POST-DATA table will be populated
with the body contents instead. Otherwise, this stream will allow you to
read the request body data and parse it as appropriate. If a stream is
returned, it must have the element-type (UNSIGNED-BYTE 8).

Consult the content-type and content-length headers for how to handle the
data encoded in this stream.

See REQUEST
See HEADER")

  (function headers
    "Accesses the hash table of headers that the request received or the response should send out.

See REQUEST
See RESPONSE")

  (function post-data
    "Accesses the hash table of POST-body variables that the request received.

keys are strings, case-insensitive. If the key ends with \"[]\", the
value is a list of payloads, otherwise a single payload. If no POST parameter
of the given key was passed on the request, the value is NIL.

A payload is either a single data string, or if the request was performed
with multipart/form-data and the parameter is a file upload, a list of
 (PATH ORIGINAL-FILENAME MIME-TYPE), the first being a pathname, the rest
being strings.

See POST-VAR
See POST/GET
See FILE
See REQUEST")

  (function get-data
    "Accesses the hash table of GET variables that the request received.

keys are strings, case-insensitive. If the key ends with \"[]\", the
value is a list of strings, otherwise a single string. If no GET parameter
of the given key was passed on the request, the value is NIL.

See GET-VAR
See POST/GET
See REQUEST")

  (function cookies
    "Accesses the table of cookies that the request received or the response should send out.

See REQUEST
See RESPONSE")

  (function domain
    "Accesses the domain that the request arrived on or the cookie is active on.

See REQUEST
See COOKIE")

  (function remote
    "Accesses the remote address that the request was sent out from.

See REQUEST")

  (function data
    "Accesses the table of internal data that the request should keep and the data that the response should send out.

For the response, the data is allowed to be of type
 PATHNAME
 STRING
 (ARRAY (UNSINGED-BYTE 8))

See REQUEST
See RESPONSE")

  (function issue-time
    "Accesses the universal-time at which the request was issued.

See REQUEST")

  (type response
    "Container class to represent a response that should be sent out for a request.

See DATA
See RETURN-CODE
See CONTENT-TYPE
See EXTERNAL-FORMAT
See HEADERS
See COOKIES
See COOKIE
See HEADER
See REDIRECT
See SERVE-FILE")

  (function return-code
    "Accesses the HTTP return code to send out.

Defaults to 200.

See RESPONSE")

  (function content-type
    "Accesses the HTTP content-type header.

For a REQUEST object, this simply acts as a shorthand for
 (HEADER \"content-type\"), which should identify the type of content
that the client sent to the server and how it should be decoded.
For a RESPONSE object, this designates the content-type header to
send out and defaults to *DEFAULT-CONTENT-TYPE*.

See *DEFAULT-CONTENT-TYPE*
See HEADER
See REQUEST
See RESPONSE")

  (function content-length
    "Retrieves the HTTP content-length header.

This returns the number of octets of content to be read from the
BODY-STREAM, or NIL if the header was absent or malformed.

Note that even if this header is set, you should not trust the returned
number unconditionally as it could be arbitrarily forged. If you need to
buffer the whole data from the body stream, you should make sure to check
an upper bound on the content-length first.

See HEADER
See REQUEST
See BODY-STREAM")

  (function external-format
    "Accesses the external-format to use to serialise the text data.

Defaults to *DEFAULT-EXTERNAL-FORMAT*

See *DEFAULT-EXTERNAL-FORMAT*
See RESPONSE")

  (type cookie
    "Container class for a cookie.

See NAME
See VALUE
See DOMAIN
See PATH
See EXPIRES
See HTTP-ONLY
See SECURE
See REQUEST
See RESPONSE
See COOKIE-HEADER
See COOKIE")

  (function value
    "Accesses the cookie value.

See COOKIE")

  (function path
    "Accesses the URI or cookie path.

See URI
See COOKIE")

  (function expires
    "Accesses when the cookie will expire, if at all.

See COOKIE")

  (function http-only
    "Accesses whether the cookie should get the http-only flag.

See COOKIE")

  (function secure
    "Accesses whether the cookie should get the secure flag.

See COOKIE")

  (function cookie-header
    "Returns a string representation of the cookie as a header.

See COOKIE")

  (function user-agent
    "Accesses the user-agent header of the request.

See HEADERS
See *REQUEST*")

  (function referer
    "Accesses the referer header of the request.

See HEADERS
See *REQUEST*")

  (function cookie
    "Accesses the cookie instance of the request.

The SETF function will construct the proper cookie instance for you.

See COOKIES
See COOKIE
See *RESPONSE*
See *REQUEST*")

  (function get-var
    "Returns the value of the GET variable of the request.

This is case-insensitive.

See GET-DATA
See *REQUEST*")

  (function post-var
    "Returns the value of the POST variable of the request.

This is case-insensitive.

See POST-DATA
See *REQUEST*")

  (function post/get
    "Returns the value of the POST or the GET variable of the request.

If both exist, then the POST variable is preferred.
This is case-insensitive.

See POST-DATA
See GET-DATA
See *REQUEST*")

  (function header
    "Accesses the value of the header in the request or response.

This is case-insensitive

See HEADERS
See *REQUEST*
See *RESPONSE*")

  (function file
    "Returns the pathname to the file that was uploaded.

Signals a NO-SUCH-POST-PARAMETER error if the given
parameter was not present as a POST parameter at all.
Signals a POST-PARAMETER-NOT-A-FILE error if the given
parameter did not represent an uploaded file.

See POST-VAR
See *REQUEST*")

  (function redirect
    "Sets the response up to cause a redirect to the given address.

By default, if a URI instance is given, the URI is
externalised.

See URI-TO-URL
See RETURN-CODE
See *RESPONSE*")

  (function serve-file
    "Sets the response up to serve the given file.

If the file does not exist, an error of type
FILE-TO-SERVE-DOES-NOT-EXIST is signalled.

The content-type, if not explicitly given, is attempted
to be automatically discovered by MIMES:MIME-LOOKUP
and falls back to application/octet-stream.

See FILE-TO-SERVE-DOES-NOT-EXIST
See MIMES:MIME-LOOKUP
See CONTENT-TYPE
See DATA
See *RESPONSE*")

  (Function request-run-time
    "Returns the number of seconds since the request was issued.

See ISSUE-TIME
See *REQUEST*"))

;; resource.lisp
(docs:define-docs
  (variable *resource-types*
    "A map from names to resource-type instances.

See RESOURCE-TYPE
See REMOVE-RESOURCE-TYPE
See LIST-RESOURCE-TYPES")

  (type resource-type
    "Container class for a resource type.

See NAME
See LOCATORS
See RESOURCE-TYPE
See DEFINE-RESOURCE-TYPE")

  (function locators
    "Accessor to the resource type's locator functions.")

  (function resource-type
    "Accessor for the resource types.

If the requested type does not exist, an error is signalled.

See *RESOURCE-TYPES*
See RESOURCE-TYPE
See REMOVE-RESOURCE-TYPE
See LIST-RESOURCE-TYPES
See DEFINE-RESOURCE-TYPE")

  (function remove-resource-type
    "Removes the given resource type, if it exists.

See *RESOURCE-TYPES*
See RESOURCE-TYPE
See LIST-RESOURCE-TYPES")

  (function list-resource-types
    "Returns a list of names of defined resource types.

See *RESOURCE-TYPES*
See RESOURCE-TYPE
See REMOVE-RESOURCE-TYPE")

  (function resource-locator
    "Accessor to a locator function on the given resource type.

Signals an error if the ident is T and there is
no default resource locator defined on the resource type.

See RESOURCE-TYPE")

  (function define-resource-type
    "Define a new resource type.

A resource-type defines a way to retrieve a certain kind of
information from a module.

If DEFAULT is given, a fallback function is defined with it.
You can retrieve this fallback by using T as the indent.

The first argument of any resource call is always the module
it was called for.

See DEFINE-RESOURCE-LOCATOR
See RESOURCE-TYPE
See RESOURCE-LOCATOR
See RESOURCE")

  (function define-resource-locator
    "Define a resource locator for a given resource type.

The arguments list can vary depending on the resource type
and even between locators for the same resource. Ultimately
the argument list is decided by the locator that ends up
being called.

Within the body, the local function CALL-DEFAULT-LOCATOR
can be used to call the default locator for this resource
type.

If an attempt is made to define a locator for an inexistent
resource type, an error is signalled.

See DEFINE-RESOURCE-TYPE
See RESOURCE")

  (function resource
    "Returns the requested resource type on the given module.

If the module does not have a specific locator, the default
locator is called. If no default exists, an error is
signalled.

The applicable structure and number of the arguments is
dependant on the locator being called.

See LIST-RESOURCE-TYPES
See DEFINE-RESOURCE-LOCATOR")

  (resource-type domain
    "Returns an internal URI that is a representation of the domain on which the module operates.")

  (resource-type api
    "Returns an internal URI that is a representation of the path on which the given API endpoint can be called.

It expects one or more arguments, where the first is the
endpoint's name and the rest are key-value pairs of the
arguments for the call.")

  (resource-type static
    "Returns an internal URI that is a representation of the requested static resource.

Requires a single argument, namely the name of the resource.")

  (resource-type page
    "Returns an internal URI that points to the requested page.

The following values are returned:
1. URI
2. QUERY-ALIST
3. FRAGMENT

The QUERY-ALIST and FRAGMENT can be used as arguments to URI-TO-URL.

By default a page with the name corresponding to the symbol
of the given name in the module's package is used if available.

However, a module or interface may special-case certain page
names to provide a specified name to point to a page of
particular interest."))

;; routing.lisp
(docs:define-docs
  (type route
    "Container class for a URI translation route.

See NAME
See DIRECTION
See PRIORITY
See TRANSLATOR
See ROUTE
See REMOVE-ROUTE
See LIST-ROUTES
See DEFINE-ROUTE")

  (function direction
    "Accessor to the direction in which the route operates.

Has to be one of :MAPPING :REVERSAL

See ROUTE")

  (function translator
    "Accessor to the route's translation function.

The function should accept a single URI object
as an argument and modify it as it sees fit.

See ROUTE")

  (variable *route-registry*
    "A table mapping names to the route instances.

Each value in the table is a cons where
the car is the mapping route and the cdr
is the reversal route, if any.

See ROUTE
See REMOVE-ROUTE
See LIST-ROUTES")

  (variable *route-mapping*
    "An optimised vector of translator functions to iterate through when mapping a URI.

See *ROUTE-REGISTRY*
See REBUILD-ROUTE-VECTORS")

  (variable *route-reversal*
    "An optimised vector of translator functions to iterate through when reversing a URI.

See *ROUTE-REGISTRY*
See REBUILD-ROUTE-VECTORS")

  (function ensure-path
    "Coerces the argument to a slash separated path if it is a list.")

  (function %route
    "Wrapper for the documentable find-function.")

  (function route
    "Accesses the route instance of the given name and direction.

Automatically calls REBUILD-ROUTE-VECTORS when
a route is changed.

See REBUILD-ROUTE-VECTORS
See *ROUTE-REGISTRY*
See REMOVE-ROUTE
See LIST-ROUTES
See ROUTE")

  (function remove-route
    "Removes the specified route, if any.

Automatically calls REBUILD-ROUTE-VECTORS

See REBUILD-ROUTE-VECTORS
See *ROUTE-REGISTRY*
See ROUTE
See LIST-ROUTES")

  (function list-routes
    "Lists all the route instances that are registered.

See *ROUTE-REGISTRY*
See ROUTE
See REMOVE-ROUTE")

  (function rebuild-route-vectors
    "Rebuilds the optimised route vectors.

This should be called whenever the route table
is changed.

Higher priority routes come first.

See LIST-ROUTES
See *ROUTE-MAPPING*
See *ROUTE-REVIERSAL*")

  (function define-route
    "Define a new route.

DIRECTION has to be one of :MAPPING :REVERSAL
where mapping routes transform URIs from the
external to the internal representation and vice-
versa.

The body should modify the URI object it receives
as it sees fit. While in general routes will be
called in the context of a request, it is not
absolutely necessary.

See ROUTE
See DEFINE-MATCHING-ROUTE
See DEFINE-TARGET-ROUTE
See DEFINE-STRING-ROUTE")

  (function extract-vars-and-tests
    "Turns the lambda-list into a 'pure' lambda-list, a list of tests, and a list of regex tests.

See WITH-DESTRUCTURING-ROUTE-BIND")

  (function with-destructuring-route-bind
    "Destructure the value-form according to the test-form and conditionally execute/bind body.

LAMBDA-LIST should be a destructuring-lambda-list with
the following special treatment. Aside from symbols,
numbers, strings, and lists are also allowed at any
position with the following effects:

  NUMBER  --- The body is only evaluated if the value at
              that position of the lambda-list is = to
              the argument it references.
  STRING  --- The body is only evaluated if the value at
              that position of the lambda-list is STRING=
              to the argument it references.
  LIST    --- The first item of the list is treated as a
              regular expression and the rest must be
              symbols that will be bound to the values of
              the capture groups of the regex.

This makes it succinct to write complex destructuring tests.

See CL-PPCRE:REGISTER-GROUPS-BIND
See EXTRACT-VARS-AND-TESTS")

  (function with-route-part-bindings
    "Ensures that body is only evaluated if the test passes.

Depending on the type of the TEST-FORM, the body
is wrapped differently.

 (EQL *)      --- The body is emitted in a PROGN.
 (INTEGER 0)  --- The value-form must match the test-form under =
 STRING       --- The value-form must match the test-form under STRING=
 LIST         --- The body is wrapped in a WITH-DESTRUCTURING-ROUTE-BIND
 SYMBOL       --- The symbol held is bound to the value-form's value.

See WITH-DESTRUCTURING-ROUTE-BIND")

  (function with-route-test-bindings
    "Binds each part of the URI for the duration of the body.

See WITH-ROUTE-PART-BINDINGS")

  (function define-matching-route
    "Defines a route where the URI is bound to URIVAR and the body is only evaluated if the tests for the individual parts of the URI pass.

See WITH-ROUTE-TEST-BINDINGS
See DEFINE-ROUTE")

  (function define-target-route
    "Defines a route that attempts to automatically translate the URI according to the given test and result parts.

The result parts can reference all variables introduced
by the test parts.

See DEFINE-MATCHING-ROUTE")

  (function define-string-route
    "Defines a route where the URI is analysed by the given regex and translated into the interpolated string representation.

The target string can reference regex capture groups.

Note that the regexes are transformed in order to make the
definition appear more \"natural\". Specifically, if no port
is present in the source, it is prefixed with

  (?:[^/]*)?

if no domain is present in source, it is prefixed with

  [^:/]*

if the target does not contain a port, the port is not changed.
The source is then surrounded by ^ and $. What this all does in
effect is that it prevents regexes from matching too liberally
for common URI changes. It allows something like this:

  /foo/bar => foo/bar

to actually match and exchange an uri like this

  localhost:8080/foo/bar

properly into the expected URI

  foo:8080/bar

without changing a URI like this

  localhost:8080/bla/foo/bar

as that would indeed not be what we expected to have specified.
If you do not like these automatic changes to the regexes, you
can easily enough define your own route that allows you full
control.

See CL-PPCRE:REGEX-REPLACE
See DEFINE-ROUTE")

  (function internal-uri
    "Modifies the URI by pushing it through all mapping routes so that it becomes an internal URI.

See *ROUTE-MAPPING*")

  (function external-uri
    "Modifies the URI by pushing it through all reversal routes so that it becomes an external URI.

See *ROUTE-REVERSAL*"))

;; standard-interfaces.lisp
(docs:define-docs
  )

;; toolkit.lisp
(docs:define-docs
  (variable *random-string-characters*
    "A string that contains all the characters that can form a random-string.

See MAKE-RANDOM-STRING")

  (variable +unix-epoch-difference+
    "The time difference between unix and universal time.")

  (function enlist
    "Ensure that VAR is a list, appending the other args if it is not yet one.")

  (function universal-to-unix-time
    "Translate the given universal-time to a unix-time

See +UNIX-EPOCH-DIFFERENCE+")

  (function unix-to-universal-time
    "Translate the given unix-time to a universal-time

See +UNIX-EPOCH-DIFFERENCE+")

  (function get-unix-time
    "Return the current time as a unix timestamp in seconds since 1970.")

  (function format-relative-time
    "Returns a string representing the given timestamp seconds as a relative time.

Bot universal-time stamp and local-time:timestamp
are accepted.

It divides the time up into seconds, minutes,
hours, days, weeks, months, years, decades,
centuries, and finally æons.

Example: 63 results in \"1 minute 3 seconds\"

See FORMAT-TIME")

  (function format-clock-time
    "Returns a string representing the given timestamp in wall-clock time.

Bot universal-time stamp and local-time:timestamp
are accepted.

The effective format is \"hh:mm:ss\"

See FORMAT-MACHINE-DATE
See FORMAT-HUMAN-DATE
See FORMAT-FANCY-DATE")

  (function format-machine-date
    "Returns a string representing the given timestamp in machine-readable date.

Bot universal-time stamp and local-time:timestamp
are accepted.

The effective format is \"YYYY-MM-DDThh:mm:ss\"

See FORMAT-CLOCK-TIME
See FORMAT-HUMAN-DATE
See FORMAT-FANCY-DATE")

  (function format-human-date
    "Returns a string representing the given timestamp in human-readable date.

Bot universal-time stamp and local-time:timestamp
are accepted.

The effective format is \"YYYY.MM.DD hh:mm:ss\"

See FORMAT-CLOCK-TIME
See FORMAT-MACHINE-DATE
See FORMAT-FANCY-DATE
See FORMAT-TIME")

  (function format-fancy-date
    "Returns a string representing the given timestamp in an extensive, fancy date.

Bot universal-time stamp and local-time:timestamp
are accepted.

The effective format is \"WEEKDAY, DAY of MONTH Y, H:MM:SS UTC\"

See FORMAT-CLOCK-TIME
See FORMAT-MACHINE-DATE
See FORMAT-HUMAN-DATE")

  (function format-time
    "Returns a string representing the given timestamp in an intuitive way.

If the difference from the current time is below
the RELATIVE-TIME-THRESHOLD, the time is formatted
relatively, otherwise absolutely.

See FORMAT-RELATIVE-TIME
See FORMAT-HUMAN-DATE")

  (function make-random-string
    "Constructs a string composed of random characters.

See *RANDOM-STRING-CHARACTERS*")

  (function file-size
    "Returns the file size in bytes.")

  (function resolve-base
    "Resolves the filesystem directory of the thing if possible.")

  (function read-value
    "Used to interactively read an evaluated form.")

  (function or*
    "Similar to OR, but treats empty strings as NIL.

This is often handy in the context of query
parameters from the outside world where an
empty string represents no value.")

  (function perm
    "Macro to encompass a permission.

You should use this wherever you reference a permission.
Using this will ensure that the permission is registered
with your module and thus inspectable from the outside.")

  (function copy-hash-table
    "Copies the given hash-table as accurately as possible.")

  (function parse-path-safely
    "Parses the given namestring as a path that only understands relative directories, name, and type.

Furthmore, no special path syntax is allowed and everything
is parsed verbatim. This avoids exploits where an URL is
turned into a pathname and uses special characters like ~
or ..")

  (function url-encode
    "Encodes the given string to the stream in url-encoded format.

This means that characters that are not one of a-Z 0-9 - . _ ~
are written down in percent-encoded schema.")

  (function rewrite-url
    "Rewrites the URL, replacing components with the given parts.

Accepts a URL string, a URI, or a PURI:URI. In the case of
a URI, the URI is treated as-is and no routing is performed.

Returns the rewritten URL as a string.

The meanings of the parameters are the same as for URIs
URI-TO-URL respectively, namely:

SCHEMA     --- The schema or scheme of the url as a string.
DOMAINS    --- A list of domains in order. Eg: (com example www)
PORT       --- The port as an integer.
PATH       --- The directory path as a string.
PARAMETERS --- Query GET parameters as an alist.
FRAGMENT   --- The URL fragment or anchor as a string.

All of these parameters should /NOT/ be passed URL-ENCODED.
This function will take care of the appropriate URL-ENCODING
if necessary.

If a parameter is passed as NIL, it means that the component
should be absent from the URL.

See URI
See URI-TO-URL
See MERGE-URL")

  (function merge-url
    "Rewrites the URL, merging the given parts where appropriate.

Accepts a URL string, a URI, or a PURI:URI. In the case of
a URI, the URI is treated as-is and no routing is performed.

Returns the rewritten URL as a string.

The meanings of the parameters are the same as for URIs
URI-TO-URL respectively, namely:

SCHEMA     --- The schema or scheme of the url as a string.
               Overrides if already existing.
DOMAINS    --- A list of domains in order. Eg: (com example www)
               These domains are prepended in the rendered URL.
               Eg: \"www\"+\"example.com\" = \"www.example.com\"
PORT       --- The port as an integer.
               Overrides if already existing.
PATH       --- The directory path as a string.
               The path will appear appended in the rendered URL.
               Eg: \"foo\"+\"/bar\" = \"/bar/foo\"
PARAMETERS --- Query GET parameters as an alist.
               The parameters are prepended in the rendered URL.
               Eg: \"a=b\"+\"a=a\" = \"a=b&a=a\"
FRAGMENT   --- The URL fragment or anchor as a string.
               Overrides if already existing.

All of these parameters should /NOT/ be passed URL-ENCODED.
This function will take care of the appropriate URL-ENCODING
if necessary.

This function can be especially useful for automating redirects.
For instance, if you would like to redirect back but add a
message that should be displayed, you can use APPEND-URL on the
REFERER and add the message with the PARAMETERS argument.

See URI
See URI-TO-URL
See REWRITE-URL")

  (function with-actions
    "A macro to help handle different actions in a submission context.

ERROR and INFO are variables that hold objects that
describe error and information messages that occurred
during the handling of the action.

First, the actual action is read out of the POST/GET
variable \"action\". Then the matching action-clause,
if any, is evaluated. If an error occurs during the
evaluation thereof, the error is stored in the ERROR
variable. After the action clause processing has
finished, the body forms are evaluated.

ACTION-CLAUSES ::= (clause body-form*)
CLAUSE         --- A string designator that names the action")

  (function check-for-shared-symbol
    "Attempts to detect whether the symbol is likely to be shared with other packages.

This signals a warning of type DEFINITION-FOR-SHARED-PACKAGE
if the symbol's package is not equal to the current *PACKAGE*.

See DEFINITION-FOR-SHARED-PACKAGE"))

;; uri.lisp
(docs:define-docs
  (type uri
    "Class to represent a URI in the system.

URIs are used to access and define resources that should
be accessible both internally and externally.

If MATCHER is T, then the matcher is automatically set
to the scanner created from the path or an empty string
should the path be NIL.

See DOMAINS
See PORT
See PATH
See MATCHER
See MAKE-URI
See COPY-URI
See URI<
See URI>
See URI=
See URI-MATCHES
See MERGE-URIS
See REPRESENT-URI
See URI-TO-URL")

  (function domains
    "Accesses the list of (sub)domains of the URI.

The domains are in order of increasing specificity.
This means they are the reverse of standard URL
syntax. This is done for ease of matching and
because it is honestly the more sensible way to
represent a domain.

See URI")

  (function port
    "Accesses the port of the URI.

Must be of type (OR (INTEGER 0 65535) NULL)

See URI")

  (function path
    "Accesses the path of the URI.

If the URI is meant to be matched against, the
path can also be a regular expression as per
cl-ppcre. In that case, the URI's MATCHER slot
must be a cl-ppcre scanner function.

See MATCHER
See URI")

  (function matcher
    "Accesses the regex matcher function of the URI.

The matcher only matches against the path part
of a URI. The matcher should always act according
to the regex stored in the PATH part of the URI.

See PATH
See URI")

  (function uri-string
    "Returns a parsable string representation of the URI.

See URI")

  (function make-uri
    "Creates a new URI instance according t o the given parts.

See URI")

  (function ensure-uri
    "Ensures that the object is a URI and returns it.

If URI-ISH is a STRING, then it is parsed.

See PARSE-URI
See URI")

  (function copy-uri
    "Creates a full copy of the URI.

If URI is a STRING, then it is parsed.

See PARSE-URI
See URI")

  (variable *uri-regex*
    "Stores the cl-ppcre parse function that is used to parse a URI from a string.")
  
  (function parse-uri
    "Parses the given URI into a string if possible.

Signals an error of type UNPARSABLE-URI-STRING
if the string cannot be parsed.

See URI")

  (function read-uri
    "Reads a URI as a string from the stream.

See PARSE-URI
See READ")

  (variable *default-uri-defaults*
    "Contains a default, neutral URI.")

  (function uri<
    "Returns T if A logically precedes B in specificity.

In more detail (short-circuiting top to bottom):
  T    A does not have a port, but B does
  T    A's number of domains is less than B's
  T    A's length of the path is greater than B's
  NIL  for everything else

See URI")

  (function uri>
    "Returns T if A logically follows B in specificity.

See URI<")

  (function uri=
    "Returns T if A and B represent the same URIs.

Specifically:
  Ports must be EQL
  Paths must be EQUAL
  Domains must be the same order and be STRING-EQUAL

See URI")

  (function uri-matches
    "Returns T if the URI matches the PATTERN-URI.

Sepcifically, in order to match:
  The matcher of the pattern must match the path of the uri.
  All the domains must be in the same order and match by
  STRING-EQUAL.
  Either one of them does not have a port set, or the
  ports must match by =.

See MATCHER
See URI")

  (function merge-uris
    "Creates a new URI by merging the given two.

See URI")

  (function represent-uri
    "Returns a new URI that is represented accordingly.

REPRESENTATION can be one of the following:
  :AS-IS NIL          --- The URI is simply copied
  :EXTERNAL :REVERSE  --- The URI is externalised
  :INTERNAL :MAP      --- The URI is internalised

See COPY-URI
See EXTERNAL-URI
See INTERNAL-URI")

  (function uri-to-url
    "Returns a URL representation of the URI.

The QUERY argument takes an alist of keys and variables
to be used in the query part of the resulting URL. The
fragment is the optional fragment part. The schema is,
unless explicitly given, automatically set to HTTPS if
the translated URI's port is 443, the value of the
X-Forwarded-Proto header if present, or HTTP otherwise.

The path, query, and fragment are url-encoded as
necessary.

See REPRESENT-URI
See URL-ENCODE
See MAKE-URL")

  (function make-url
    "A shorthand function for constructing URL strings.

This basically encompasses the combination of a MAKE-URI
and a URI-TO-URL rather directly. The representation is
set to :EXTERNAL for convenience as that is usually the
preferred option for use-cases of this function.

See MAKE-URI
See URI-TO-URL")

  (function format-uri
    "A shorthand function to construct a URI with a format string.

This is basically just the following:
  (parse-uri (format NIL ..))

See PARSE-URI"))

;;; Interfaces
;; Ban
(docs:define-docs
  (function ban:jail
    "Ban the given IP.

The IP may be in IPv4 or IPv6 format as a string.

The duration must be either an integer denoting the
number of seconds to ban for, or T in which case the
duration is taken as being infinite. If no duration is
given, an implementation-dependant default is chosen.

If the IP is already banned, the ban duration is extended
by the given amount.

See BAN:LIST
See BAN:RELEASE")

  (function ban:list
    "Returns a list of banned IP addresses.

See BAN:JAIL-TIME")
  
  (function ban:jail-time
    "Returns the number of seconds the IP has left on its ban, T for infinity, or NIL for none.

See BAN:JAIL
See BAN:RELEASE")

  (function release
    "Unbans the given IP.

See BAN:JAIL"))

;; Rate
(docs:define-docs
  (function rate:define-limit
    "Define the behaviour for a rate limitation.

TIME-LEFT will be bound to the number of seconds left
until the timeout is lifted. The body is the code that
will be evaluated when the rate limit was hit. It
should probably display an error page and perhaps
record the infraction.

TIMEOUT should be the interval of rate limiting. LIMIT
is the number of attempts that can be made within the
TIMEOUT. More straight-forwardly put: A timeout of 30
and limit of 3 means a user can make 3 attempts
within 30 seconds. If he tries to make more, he is
blocked until the time since the last attempt has
reached TIMEOUT seconds.

See RATE:LEFT
See RATE:WITH-LIMITATION")

  (function rate:left
    "Returns two values, the number of attempts left, and the number of seconds left until the timeout is over.

The user is rate limited if the number of attempts left
is zero.")

  (function rate:with-limitation
    "Evaluates the body within a rate limited environment.

The limit must be defined beforehand.

The rules defined by the named limit are applied. If
the user exceeds the allowed number of attempts and is
within rate limitation, the body is not evaluated.

See RATE:LEFT
See RATE:DEFINE-LIMIT"))

;; Admin
(docs:define-docs
  (function admin:list-panels
    "Returns an alist of panel categories and their panels.

See ADMIN:DEFINE-PANEL
See ADMIN:REMOVE-PANEL")

  (function admin:remove-panel
    "Removes the specified panel if it exists.

See ADMIN:DEFINE-PANEL")

  (function admin:define-panel
    "Defines a new administration panel.

Category and panel names are case-insensitive.

This is similar to DEFINE-PAGE, except it defines a panel
within the administration interface. The body code should
return HTML data to be emitted into the interface. Do not
emit root elements such as <HTML> or <BODY>.

The panel definition can be extended through the ADMIN:PANEL
option. By default, the following options are guaranteed to
exist:

  :ACCESS   Will restrict access to it to users that pass
            the permission check. See USER:CHECK.
  :ICON     Selects an icon to use for the panel in the
            administration's menu. The accepted range of
            values is implementation-dependant.
  :TOOLTIP  A tooltip string to show when hovering over the
            panel entry in the administration's menu.

In order to obtain the internal URI to an admin panel, use
the PAGE resource with the category and panel names as
arguments.

See ADMIN:LIST-PANELS
See ADMIN:REMOVE-PANEL"))

;; Cache
(docs:define-docs
  (function cache:get
    "Returns the contents cached under the given name and variant, if any.

See CACHE:WITH-CACHE")

  (function cache:renew
    "Clears out the cache under the given name and variant and potentially refreshes it.

The refresh may be deferred until the next time the associated
WITH-CACHE form is evaluated.

See CACHE:GET
See CACHE:WITH-CACHE")

  (function cache:with-cache
    "Caches the return value produced by the body under the name.

The name is not evaluated and must be a direct symbol, but the
variant is evaluated.

Every time this form is evaluated, the TEST-FORM is evaluated.
If it returns non-NIL, the body is evaluated and its return
value is cached under the given name. If it returns T and the
body has not yet been cached, then it is evaluated as well. 
Otherwise, the body is not evaluated and the cached value is
returned instead.

See CACHE:RENEW
See CACHE:GET"))

;; Auth
(docs:define-docs
  (variable auth:*login-timeout*
    "The time, in seconds, for which an authenticated user should remain authenticated.")

  (hook auth:associate
    "This hook is called when a session is associated with a user.

The hook has an argument, the session object. You can retrieve
the associated user through AUTH:CURRENT.

See AUTH:CURRENT")

  (hook auth:no-associated-user
    "This hook is called when AUTH:CURRENT is called but no user is found in the session.

The hook has an argument, the session object.

Triggers on this hook may associate new users at this time by
using AUTH:ASSOCIATE. If a new user is associated by this hook,
this user is returned by the original AUTH:CURRENT call.

This allows third-party libraries to implement additional
authentication mechanisms.

See AUTH:CURRENT")

  (function auth:current
    "Returns the user object that is authenticated with the given session.

If no session is passed, the one of the current request is
retrieved by SESSION:GET.

If the session is not authenticated, the user designated
by DEFAULT is returned instead, if given. If DEFAULT is
NIL, then NIL is returned.

See SESSION:GET
See AUTH:ASSOCIATE")

  (function auth:associate
    "Associates (authenticates) the user with the given session.

If no session is passed, the one of the current request is
retrieved by SESSION:GET.

This effectively logs the session in as the user and makes it
gain all permissions the user has. If the session was associated
with a user already, the associated user is replaced.

The auth interface must also provide a way for a user to
authenticate themselves through a page. You can gain an internal
URI to that page by the page resource and the page name \"login\".
It accepts a single extra argument, which is t he URI or URL to
redirect to after a successful login. If this argument is the
string \"#\" the value of the current request's REFERER is used.

The exact means by which a user can authenticate themselves on
said page are implementation dependant.

See SESSION:GET
See AUTH:CURRENT"))

;; Session
(docs:define-docs
  (variable session:*default-timeout*
    "The default number of seconds a session can stay live for without being used.

See SESSION:TIMEOUT")

  (hook session:create
    "This hook is triggered when a new session object is started.

The session object is passed as an argument.")

  (type session:session
    "The session object that encapsulates the connection a particular client has to the system.

A session uniquely identifies a \"client\" that connects to the
server. The means by which clients are distinguished from each-
other and the means by which associated sessions are tracked are
implementation-dependant.

A session is \"active\" for a certain period of time. This timeout
is reset every time a request is made to the server by the client
that is tied to the session. Once the timeout is reached, the
session object is no longer considered active and may be removed
entirely at any time.

Sessions must be able to store arbitrary fields that can be used
to store data associated with the session. Each session also has
a globally unique ID string that can be used to coerce session
objects to storage.

See SESSION:=
See SESSION:GET
See SESSION:LIST
See SESSION:ID
See SESSION:FIELD
See SESSION:TIMEOUT
See SESSION:END
See SESSION:ACTIVE-P")

  (function session:=
    "Compares the two session objects and returns true if they represent the same session identity.

See SESSION:SESSION")

  (function session:start
    "Starts a new session in the current request context and returns it.

If the request already has a session associated with it, it is
replaced.

Once a session has been started, the client must receive
the same session on future requests to the server. How this
client tracking is achieved is implementation-dependant.

See SESSION:SESSION
See SESSION:GET
See SESSION:END")

  (function session:get
    "Retrieves the current session, or a particular one.

If no SESSION-ID is given, the session associated with the
current client is returned, if any. The SESSION-ID must be
a string that is STRING= to one previously obtained by a call
to SESSION:ID.

See SESSION:SESSION
See SESSION:ID")

  (function session:list
    "Returns a fresh list of all known session objects.

The sessions may not all be active.

Invoking this function may be expensive.

See SESSION:SESSION
See SESSION:ACTIVE-P")

  (function session:id
    "Returns the globally unique ID of the session as a string.

If no SESSION-ID is given, the session associated with the
current client is used.

The ID must be unique in such a way that no two sessions that are
known to the implementation at any time may have the same ID.

See SESSION:SESSION")

  (function session:field
    "Accessor to an arbitrary data storage field for a session object.

If no SESSION-ID is given, the session associated with the
current client is used.

The keys must be objects comparable under EQL.

See SESSION:SESSION")

  (function session:timeout
    "Accessor to the timeout, in seconds, of the session object.

If no SESSION-ID is given, the session associated with the
current client is used.

See SESSION:SESSION
See SESSION:ACTIVE-P")

  (function session:end
    "Ends the session immediately, making it inactive.

See SESSION:SESSION
See SESSION:START
See SESSION:ACTIVE-P")

  (function session:active-p
    "Returns whether the session is currently active or not.

An inactive session will no longer be tied to any future
client requests and may be removed at any time.

See SESSION:SESSION"))

;; User
(docs:define-docs
  (type user:condition
    "Base condition type for conditions related to the user interface.")

  (type user:not-found
    "Condition signalled when an unknown user was requested.

See USER:CONDITION
See USER:GET")

  (hook user:create
    "This hook is triggered when a new user is created.

The user object is passed as an argument.")

  (hook user:remove
    "This hook is triggered when an existing user is removed.

The user object is passed as an argument.")

  (hook user:ready
    "This hook is called when the user system has become ready and users can be accessed.

You should refrain from performing user operations while
this hook is untriggered.

Note that this is a sticky hook and will remain active
until USER:UNREADY is triggered.")

  (hook user:unready
    "This hook is called when the user system is no longer ready and users can no longer be accessed.

Note that this causes the USER:READY hook to become
unstickied.")

  (type user:user
    "Container for a user.

A user is an object that represents a certain person with
access to the system. Each user has a uniquely identifying
username string of 1-32 case-insensitive characters of the
following range:

  \"abcdefghijklmnopqrstuvwxyz0123456789_-.\"

Each user object can store arbitrary information in fields.

A user has a set of permissions that describe which
protected resources the user should be granted access to.
These permissions are laid out in the form of trees,
where each permission describes a certain path down a
tree. For example, the following permission

  foo.bar

would grant access to the permissions foo.bar, foo.bar.baz
and so forth, but not to foo or foo.bam . Permissions can
be denoted by either dot-delimited strings, or lists.

Note that the user system may need to be readied up first
before it can be reliably used. Attempting to perform any
user operations before the USER:READY hook has been
triggered or after the USER:UNREADY hook has been triggered
will result in undefined behaviour.

A user object is not required to sync with the backend, and
in fact does not need to be identical according to EQ to
another user object for the same user. It is thus not a
good idea to retain and cache user objects, as the user's
attributes might change asynchronously, causing a previous
instance to become outdated.

See PERM
See USER:=
See USER:LIST
See USER:GET
See USER:ID
See USER:USERNAME
See USER:FIELDS
See USER:FIELD
See USER:REMOVE
See USER:CHECK
See USER:GRANT
See USER:REVOKE")

  (function user:=
    "Compares two user object to see if they denote the same user.

See USER:USER")

  (function user:list
    "Returns a fresh list of all known user objects.

This may be a very costly operation.

See USER:USER")

  (function user:get
    "Returns the requested user object.

The user can be referenced by either a username as a
string, or a user ID as an integer.

IF-DOES-NOT-EXIST may be one of the following values,
which decides on the behaviour in the case where the
requested user is not known.

  :CREATE     The user is created.
  :ERROR      An error of type USER:NOT-FOUND is signalled.
  :ANONYMOUS  The anonymous standard user is returned.
  NIL         NIL is returned.

The :CREATE option is only valid if the given identifier
is a string denoting a username. If :CREATE is used with
a user ID, an error is signalled.

When a new user is created, the USER:CREATE hook is
triggered.

The user named \"anonymous\" must always exist and is
automatically created by the implementation. It is
intended to be used for unauthenticated users.

Note that fetching users while the user system is not
yet ready will result in undefined behaviour.

See USER:READY
See USER:UNREADY
See USER:USER")

  (function user:id
    "Returns the integer ID of the user object.

This is useful for references to user accounts from
database records, as storing the full username is both
wasteful and inefficient for searching.

See USER:USER")

  (function user:username
    "Returns the username of the requested user.

See USER:USER")

  (function user:fields
    "Returns an alist of field names to values of the user.

See USER:USER
See USER:FIELD
See USER:REMOVE-FIELD")

  (function user:field
    "Accessor to a field on the user.

Keys must be strings of a maximum length of 64.
Values must be strings, but can be of arbitrary length.

See USER:USER
See USER:REMOVE-FIELD
See USER:FIELDS")

  (function user:remove-field
    "Removes the requested field from the user.

See USER:USER
See USER:FIELD
See USER:FIELDS")

  (function user:remove
    "Removes the given user.

This deletes all associated data of the user.

On successful deletion, the USER:REMOVE hook is triggered.

See USER:USER")

  (function user:check
    "Checks whether the user is permitted access to the given permission branch.

See USER:USER for a description of permission branches.

Note that a permission is granted as long as there is
a granted permission on the user that is /higher/ than
that which is checked against. Eg, if the user has the
permission of foo granted, then checking whether any of
foo.bar, foo.bar.baz, etc. are granted will yield true.

See USER:USER
See USER:GRANT
See USER:REVOKE")

  (function user:grant
    "Grants the user access to the given permission branches.

See USER:USER
See USER:CHECK
See USER:REVOKE")

  (function user:revoke
    "Revokes access from the given permission branches.

See USER:USER
See USER:CHECK
See USER:GRANT")

  (function user:add-default-permissions
    "Adds the given branches as default permissions to be granted to newly created users.

Note that this will not retroactively grant permissions and
only comes into effect for users created after a call to
this has been run.

See USER:USER
See USER:GRANT"))

;; Mail
(docs:define-docs
  (hook mail:send
    "This hook is triggered right before a mail is sent out.

The recipient, subject, and body text are passed as arguments.

See MAIL:SEND")
  
  (function mail:send
    "Sends an email to the specified address.

If the sending of the email should fail, an error must
be signalled.

This function may be extended with additional keyword
arguments if the implementation supports extraneous
information, such as additional headers, attachments, and
so forth."))

;; Profile
(docs:define-docs
  (function profile:avatar
    "Returns a full URL to an avatar image for the user.

The size of the image should approximate the one passed
as an argument, in pixels. It must however not be exact.
You have to take care to scale the image to the appropriate
size on the client regardless. The size specified here is
only useful to optimise the image size for transfer speed.")
  
  (function profile:name
    "Returns the \"display name\" of the user.

The display name is a name that, unlike the username,
can be changed and the user deems preferable over the
username.")

  (function profile:fields
    "Returns a list of defined fields.

Each field is an alist with the following keys defined:
 :NAME :TYPE :DEFAULT :EDITABLE

The actual value of the fields can be accessed through the
USER:FIELD accessor.

See PROFILE:ADD-FIELD
See PROFILE:REMOVE-FIELD
See USER:FIELD")

  (function profile:add-field
    "Adds a custom profile field that is visible on the user's profile.

These fields can be used to store public information such
as counters, stats, birthday, bio, etc.

If EDITABLE is non-NIL, the user is allowed to change the
field themselves.

TYPE can be one of the following:
  text textarea password email url time date datetime-local
  month week color number range checkbox radio file tel

The type is mostly useful for the display presentation to
the user. The actual lisp data type used to store the field
is a string.

Note that this only stores the intended interpretation of a
field, not the field itself.

See PROFILE:FIELDS
See PROFILE:REMOVE-FIELD")

  (function profile:remove-field
    "Removes the given field information, if it exists.

See PROFILE:ADD-FIELD
See PROFILE:FIELDS")

  (function profile:list-panels
    "Returns a list of profile panel names.

See PROFILE:REMOVE-PANEL
See PROFILE:DEFINE-PANEL")

  (function profile:remove-panel
    "Removes the panel of the given name.

See PROFILE:LIST-PANELS
See PROFILE:DEFINE-PANEL")

  (function profile:define-panel
    "Defines a new panel to be shown on the user profile.

This is similar to DEFINE-PAGE, except it defines a panel
within the user profile interface. The body code should
return HTML data to be emitted into the interface. Do not
emit root elements such as <HTML> or <BODY>.

The panel definition can be extended through the PROFILE:PANEL
option. By default, the following options are guaranteed to
exist:

  :ACCESS   Will restrict access to it to users that pass
            the permission check. See USER:CHECK.

In order to obtain the internal URI to a profile panel, use
the PAGE resource with the user and panel name as arguments.

See PROFILE:LIST-PANELS
See PROFILE:REMOVE-PANEL"))

;; Server
(docs:define-docs
  (hook server:started
    "This hook is triggered right after the server has been started.

Note that this is a sticky hook and will remain active
until SERVER:STOPPED is triggered.")

  (hook server:stopped
    "This hook is triggered right after the server has been stopped.

Note that this causes the SERVER:STARTED hook to become
unstickied.")
  
  (function server:start
    "Starts a listener server.

The name must be a keyword.

Note that the implementation must include additional
keyword arguments that specify the actual instance
behaviour such as the port, address, and so forth. However,
in order to also allow exotic servers such as pipe-based
ones, no such arguments are specified.

See the documentation of your implementation of choice.

If a server that is similar to the requested one (by name
or the provided options), is already started, an error is
signalled.

On successful start, the SERVER:STARTED hook is triggered.
On unsuccessful start, an error is signalled.

The server implementation must take care to invoke REQUEST
with the proper arguments on an incoming HTTP request, and
to send the data contained in the returned response object
back.

See SERVER:STOP
See SERVER:LISTENERS
See REQUEST
See RESPONSE")

  (function server:stop
    "Stops the listener server on the given PORT.

The name must be a keyword.

If no listener with the requested name is found, an error
may be signalled.

On successful stop, the SERVER:STOPPED hook is triggered.
On unsuccessful stop, an error is signalled.

See SERVER:START
See SERVER:LISTENERS")

  (function server:listeners
    "Returns a list of active listener instances.

See SERVER:START
See SERVER:STOP"))

;; Logger
(docs:define-docs
  (function logger:log
    "Logs the given message under the level.

LEVEL has to be one of :TRACE :DEBUG :INFO :WARN :ERROR :SEVERE :FATAL.

Note that the exact means by which the message is presented
or logged is up to the implementation.

See LOGGER:TRACE
See LOGGER:DEBUG
See LOGGER:INFO
See LOGGER:WARN
See LOGGER:ERROR
See LOGGER:SEVERE
See LOGGER:FATAL")
  
  (function logger:trace
    "Logs the given message in the category as a trace message.

See LOGGER:LOG")
  
  (function logger:debug
    "Logs the given message in the category as a debug message.

See LOGGER:LOG")
  
  (function logger:info
    "Logs the given message in the category as an information message.

See LOGGER:LOG")
  
  (function logger:warn
    "Logs the given message in the category as a warning message.

See LOGGER:LOG")
  
  (function logger:error
    "Logs the given message in the category as an error message.

See LOGGER:LOG")
  
  (function logger:severe
    "Logs the given message in the category as a severe message.

See LOGGER:LOG")
  
  (function logger:fatal
    "Logs the given message in the category as a fatal message.

See LOGGER:LOG"))

;; Database
(docs:define-docs
  (type database:condition
    "Base condition type for all database related conditions.

See DATABASE:CONNECTION-FAILED
See DATABASE:CONNECTION-ALREADY-OPEN
See DATABASE:COLLECTION-CONDITION
See DATABASE:INVALID-COLLECTION
See DATABASE:COLLECTION-ALREADY-EXISTS
See DATABASE:INVALID-FIELD")
  
  (type database:connection-failed
    "Error signalled upon a failed connection attempt.

See DATABASE:CONDITION")
  
  (type database:connection-already-open
    "Warning signalled when a new connection is established while an old one was already open.

See DATABASE:CONDITION")
  
  (type database:collection-condition
    "Base condition type for collection related conditions.

See DATABASE:CONDITION
See DATABASE:INVALID-COLLECTION
See DATABASE:COLLECTION-ALREADY-EXISTS
See DATABASE:INVALID-FIELD")
  
  (type database:invalid-collection
    "Error signalled when an invalidly named, or inexistent collection is accessed.

See DATABASE:COLLECTION-CONDITION")
  
  (type database:collection-already-exists
    "Error signalled when a new collection is attempted to be created where an old one already exists.

See DATABASE:COLLECTION-CONDITION")
  
  (type database:invalid-field
    "Error signalled when an invalid field name or type is used.

See DATABASE:COLLECTION-CONDITION")

  (type database:id
    "Effective type of values for ID-type fields in a collection.

See DATABASE:ENSURE-ID")

  (function database:ensure-id
    "Coerces the given string into an ID, if possible.

May signal an error on invalidly structured or invalid ID.

The return value is of type DATABASE:ID

See DATABASE:ID")

  (hook database:connected
    "This hook is triggered after the database has been connected.

The name of the connection is passed as an argument.

Note that this is a sticky hook and will remain active
until DATABASE:DISCONNECTED is triggered.")

  (hook database:disconnected
    "This hook is triggered after the database has been disconnected.

The name of the connection is passed as an argument.

Note that this causes the DATABASE:CONNECTED hook to become
unstickied.")
  
  (function database:connect
    "Establishes a connection to the database of the given name.

The database connection must be configured by some 
implementation-defined manner beforehand. If the
implementation cannot automatically configure itself,
or cannot find a database of the requested name, an
error of type DATABASE:CONNECTION-FAILED is signalled.

If a previous connection is already open and a new one is
attempted, a warning of type DATABASE:CONNECTION-ALREADY-OPEN
is signalled, the previous connection is closed, and the
new one is opened.

If the connection fails for some other reason, an error
of type DATABASE:CONNECTION-FAILED is signalled.

Upon successful connection, the DATABASE:CONNECTED hook
is triggered.

Attempting to perform any database operations aside from
connecting or disconnecting before a connection has been
established results in undefined behaviour.

See DATABASE:CONNECTED
See DATABASE:DISCONNECT
See DATABASE:CONNECTION-FAILED
See DATABASE:CONNECTION-ALREADY-OPEN")

  (function database:disconnect
    "Closes the current database connection, if any.

Upon successful disconnect, the DATABASE:DISCONNECTED
hook is triggered.

See DATABASE:CONNECT
See DATABASE:DISCONNECTED")

  (function database:connected-p
    "Returns true if the database is currently connected.

See DATABASE:CONNECT
See DATABASE:DISCONNECT")

  (function database:collections
    "Returns a list of names for all known collections.

The names may be either symbols or strings.

See DATABASE:CREATE")

  (function database:collection-exists-p
    "Returns true if a collection of the given name already exists.

Signals an error of type INVALID-COLLECTION if the name
is not of the proper collection name format.

See DATABASE:CREATE")

  (function database:create
    "Creates a new collection on the database.

COLLECTION must be a valid collection name. Namely, it
must be either a symbol or a string, limited to the
following set of characters:

  ABCDEFGHIJKLMNOPQRSTUVWXYZ
  abcdefghijklmnopqrstuvwxyz
  0123456789-_/

Symbol names are coerced to string names in the following
manner: A slash and the name of the symbol's package are
prepended to the symbol's name. Thus, a symbol's name and
package must be limited to the above set of characters in
order to be usable as a collection name.

The structure denotes the schema of the collection and
must be of the following structure:

  STRUCTURE ::= (FIELD*)
  FIELD     ::= (NAME TYPE)
  TYPE      ::= ID | :text | :float | :boolean | VARCHAR
              | INTEGER
  ID        ::= :id | (:id COLLECTION)
  INTEGER   ::= :integer | (:integer BYTES)
  VARCHAR   ::= (:varchar LENGTH)

A valid name can be either a symbol or a string, where
the symbol's name/string must underlie the same character
restrictions as a collection name.

The types of a field are specified as follows:

   :ID       An implementation-defined type that is used
             to uniquely identify a record within a
             collection.
   :TEXT     A string of arbitrary length.
   :FLOAT    A double-float.
   :BOOLEAN  A boolean (NIL or T) value.
   :INTEGER  A signed integer within the range denoted by
             the number of bytes of its size in one's
             complement. Attempting to store a value
             outside of this range leads to undefined
             behaviour. The default number of bytes is 4.
             The maximum number of bytes that can be
             requested is 8.
   :VARCHAR  A string of variable size but with a maximum
             length. Attempting to store a string beyond
             that length leads to undefined behaviour.

If an invalid field name or field type is specified, an
error of type DATABASE:INVALID-FIELD is signalled.

The implementation is allowed to upgrade any field type
to a type that can encompass the specified range and more.
For example, an implementation is permitted to upgrade an
\(:integer 4) to an (:integer 8) or a (:varchar 5) to :text.

If a COLLECTION is passed to an :ID field, the field is
called a \"reference field\", and the ID must be of a record
in the referenced collection. Upon inserting or updating a
record with a reference field, the implementation may check
that the referenced record does exist and, if it does not
exist, will signal an error. Similarly, an implementation 
may delete records with a reference field if the record being
referenced is deleted. This is commonly referred to as
cascading deletes. Note that an implementation either must
always perform the check or never, and must always perform
the cascading delete or never. It must be predictable in the
behaviour, if the behaviour is specified.

The implementation may support additional field types not
specified here.

The implementation may or may not permit you to store data
in fields outside of the ones specified in the structure
of the collection.

Each collection will always include a field called \"_id\"
of type :ID that is filled by the implementation on record
creation. IT stores the unique ID of the record within the
collection. The concrete type of the ID is implementation-
dependant.

INDICES is a list of field names that should be indexed
for fast access. The implementation is urged to optimise
access to the fields, but is not required to.

IF-EXISTS denotes the behaviour in case a collection of
the same name already exists. The behaviour is as follows:

  :IGNORE     Return NIL. This is the default.
  :ERROR      Signal an error of type DATABASE:COLLECTION-ALREADY-EXISTS.
  :SUPERSEDE  Drop the old collection and create a new one.

If a new collection is created successfully, a non-NIL
value is returned.

See DATABASE:ID
See DATABASE:STRUCTURE
See DATABASE:COLLECTIONS
See DATABASE:COLLECTION-EXISTS-P
See DATABASE:DROP
See DATABASE:INVALID-COLLECTION
See DATABASE:COLLECTION-ALREADY-EXISTS
See DATABASE:INVALID-FIELD")

  (function database:structure
    "Returns the structure of the collection.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

Note that the value returned by this does not have to be
identical to the structure used to create the collection.
The types in the returned structure may be upgraded
variants of the defined types. The types returned do not
have to correspond to the concrete types used to actually
store the data.

See DATABASE:CREATE")

  (function database:empty
    "Clears all records from the collection, making it empty.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

Note that this may also clear the ID sequence, meaning
that IDs produced for records after the emptying of the
collection may be equal to IDs produced before the
emptying.

See DATABASE:CREATE")

  (function database:drop
    "Deletes the collection entirely, including all of its data.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE")

  (function database:iterate
    "Iterates over the records in the collection that match the clauses.

Effectively, FUNCTION is called with a single argument--
a hash-table filled with the requested field values-- for
each record in the collection that satisfies the QUERY.
The fields in the table are keyed by their name as a
string with the case converted downwards. The consequences
of calling another DATABASE function from within the
FUNCTION are implementation dependant.

The QUERY must be a value returned by the DATABASE:QUERY
macro.

If FIELDS is NIL, all fields are included. Otherwise it
can be a list of field names to include in each record.
The database may store more fields than were requested,
but never less.

If UNIQUE is non-NIL and FIELDS is a list of at least one
field, records are only considered to match if the same
record (each field compared under EQUAL) has not appeared
previously.

If SORT is NIL, the order in which the records are passed
is unpredictable and may be random with every call.
Otherwise, SORT should be a list of lists, where each
inner list should contain the name of a field, and either
:ASC or :DSC for ascending or descending order.

SKIP specifies the number of matching records to skip.

If AMOUNT is NIL, all matching records are passed.
Otherwise it poses an upper bound on the number of records
that the function is called with.

If ACCUMULATE is non-NIL, then the values returned by the
function call are accumulated into a list and returned
by the DATABASE:ITERATE call.

If an invalid or inexistent field name is specified, an
error of type DATABASE:INVALID-FIELD is signalled.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE
See DATABASE:SELECT
See DATABASE:QUERY
See DATABASE:INVALID-COLLECTION
See DATABASE:INVALID-FIELD")

  (function database:select
    "Returns a list of hash-tables for the records that match the clauses.

See DATABASE:ITERATE for an explanation of the options.

The QUERY must be a value returned by the DATABASE:QUERY
macro.

If an invalid or inexistent field name is requested, an
error of type DATABASE:INVALID-FIELD is signalled.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE
See DATABASE:ITERATE
See DATABASE:QUERY
See DATABASE:INVALID-COLLECTION
See DATABASE:INVALID-FIELD")

  (function database:count
    "Returns the number of records in the collection that match the query.

The QUERY must be a value returned by the DATABASE:QUERY
macro.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE
See DATABASE:QUERY
See DATABASE:INVALID-COLLECTION")

  (function database:insert
    "Inserts a new record into the collection.

The DATA can be formatted as either an alist or a hash-
table. If the data includes fields that are not defined
in the structure of the collection, the implementation
may signal an error of type DATABASE:INVALID-FIELD. If
the data does not include a field that is defined in the
structure of the collection, the field's value is set to
NIL.

If a data value is specified that does not match the
field's type, an error of type DATABASE:INVALID-FIELD
may be signalled.

If the insertion of the record fails for whatever reason,
an error of type DATABASE:CONDITION is signalled.

On successful insert, the value of the \"_id\" field for
the newly created record is returned.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE
See DATABASE:CONDITION
See DATABASE:INVALID-COLLECTION")

  (function database:remove
    "Removes the records in the collection that match the clauses.

See DATABASE:ITERATE for an explanation of the options.

If the removal of the records fails for whatever reason,
an error of type DATABASE:CONDITION is signalled.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE
See DATABASE:ITERATE
See DATABASE:INVALID-COLLECTION")

  (function database:update
    "Updates the records in the collection that match the clauses with the new data.

See DATABASE:ITERATE for an explanation of the options.

The DATA can be formatted as either an alist or a hash-
table. If the data includes fields that are not defined
in the structure of the collection, the implementation
may signal an error of type DATABASE:INVALID-FIELD. If
the data does not include a field that is defined in the
structure of the collection, the field's value is not
changed.

If a data value is specified that does not match the
field's type, an error of type DATABASE:INVALID-FIELD
may be signalled.

If the updating of the records fails for whatever
reason, an error of type DATABASE:CONDITION is signalled.

If an invalid or inexistent collection name is passed, an
error of type DATABASE:INVALID-COLLECTION is signalled.

See DATABASE:CREATE
See DATABASE:ITERATE
See DATABASE:INVALID-COLLECTION")

  (function database:with-transaction
    "Performs the database operations in its body within a transaction that ensures coherence.

If two transactions occur in parallel that would each
modify the same set of data and conflict in some manner,
one of the two transactions must be aborted by way of
an error being signalled.

If the transaction body is exited abnormally, which is
to say exited before the last form in the body has
finished execution and can return its value, all the
database operations that were performed within the body
are reverted in such a way that it appears as if they
had never been performed in the first place.")

  (function database:query
    "Constructs a value to be used for the QUERY argument in database operations.

The query form must be of the following structure:

FORM     ::= COMBINE | CLAUSE | :all
COMBINE  ::= (:and FORM*) | (:or FORM*) | (:not FORM)
CLAUSE   ::= (OPERATOR VALUE VALUE) | (:in VALUE VALUE*)
           | (:NULL VALUE)
OPERATOR ::= := | :/= | :> | :< | :<= | :>= | :MATCHES
           | :MATCHES*
VALUE    ::= (:field NAME) | 'NAME | LFORM
NAME     --- A field name.
LFORM    --- A lisp form that evaluates to a lisp value.

Where the combinators have the following effect:

  :AND  The form is true if all of the subforms are true.
  :OR   The form is true if one of the subforms is true.
  :NOT  The form is true if the subform is not true.

And the clauses have the following comparison behaviour:

  :=        True if the two values are EQUAL.
  :/=       True if the two values are not EQUAL.
  :>        True if the two values are > if they are both
            of type :integer or :float, or STRING> if they
            are both of type :varchar or :text.
  :<        True if the two values are < if they are both
            of type :integer or :float, or STRING< if they
            are both of type :varchar or :text
  :<=       True if the two values are <= if they are both
            of type :integer or :float, or STRING<= if they
            are both of type :varchar or :text
  :>=       True if the two values are > if they are both
            of type :integer or :float, or STRING>= if they
            are both of type :varchar or :text
  :NULL     True if the value is unset.
  :MATCHES  True if the first value matches the regular
            expression of the second value. The extent of
            regular-expression syntax support is up to the
            implementation, but the following basic operators
            must be understood:
              . [] [^] * + ? () {} \\ ^ $ |
            The following character classes must be understood
            as well:
              \\d \\w \\s \\D \\W \\S
  :MATCHES* Same as :MATCHES except the match is performed in
            case insensitive mode.
  :IN       True if the first value is EQUAL to one of the
            remaining values.

If the form does not correspond to the above description,
an error is signalled at macro expansion time.

Note that a special exception is made in the case of fields
of type ID, where the equality comparison (:=, :!=, :IN) is
performed in an implementation-dependant manner. The other
comparison operators cannot be used with ID type fields.

If the form is simply :all, all records in the collection
are returned.

Examples for queries:

   (db:query (:= 'name \"Hans\"))

Matches all records with a name field of \"Hans\".

   (db:query (:= '_id id))

Matches the record with the ID equivalent to the one stored
in the ID variable.

   (db:query (:and (:= 'health 0) (:= 'lives 0)))

Matches all records where the health and lives fields are 0.

   (db:query (:IN 'name \"Guybrush\" \"Elaine\" \"LeChuck\"))

Matches all records where the name is one of \"Guybrush\",
\"Elaine\", or \"LeChuck\".

   (db:query (:MATCHES 'tags \"^foo$|^foo,|,foo$|,foo,\"))

Matches all records whose tags includes the \"foo\" tag.

See DATABASE:CREATE
See DATABASE:ITERATE
See DATABASE:SELECT
See DATABASE:COUNT
See DATABASE:REMOVE
See DATABASE:UPDATE"))

;; Relational Database
(docs:define-docs
  (function relational-database:join
    "Constructs a value to be used for the COLLECTION argument in database operations.

Using this for a COLLECTION is as if there were a real
collection that had the joined records as designated by this
construct.

LEFT-COLLECTION and RIGHT-COLLECTION must follow this syntax:

  COLLECTION ::= NAME | (OPERAND OPERAND &optional TYPE)
  TYPE       ::= :INNER | :LEFT | :RIGHT | :OUTER
  OPERAND    ::= (COLLECTION FIELD)
  NAME       --- A collection name.
  FIELD      --- A field name.

The COLLECTION recursion allows joining more than two collections
together.

TYPE designates the type of join and may take the following values:

  :INNER  --- Only records that match in both left and right are
              included in the result.
  :LEFT   --- Include all records from the left table and those that
              match in the right collection. Fields from the right
              side on records that did not match will be NIL.
  :RIGHT  --- Include all records from the right collection and those
              that match in the left collection. Fields from the left
              side on records that did not match will be NIL.
  :OUTER  --- Include all records from the left and right
              collections. Fields from the left or right side on
              records that do not have a matching left or right side
              will be NIL.

The matching mentioned here only relates to the respective FIELD of
both sides being considered equal (by := of a QUERY), not to any
possible additional constraint on the records imposed by a QUERY
argument passed to the database operation the JOIN was passed to.

Fields from both the left and right hand side of a join are merged
into the same namespace. The consequences of accessing a field,
be that through a QUERY or by reading it out of the resulting
records, with a name that exists in both the left and right
collections is implementation dependant.

See DATABASE:QUERY
See DATABASE:ITERATE
See DATABASE:SELECT
See DATABASE:COUNT
See DATABASE:REMOVE
See DATABASE:UPDATE"))
