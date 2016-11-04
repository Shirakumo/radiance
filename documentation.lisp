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

See API-FORMAT")

  (function api-output
    "Emits the given data as a response.

This function should be called for any and all return data
from an api endpoint. It ensures the data proper structure,
failure handling, and data translation to the desired output
format.

See API-FORMAT")
  
  (function api-serialize
    "Cause the object to be serialised into a more favourable structure.

This function should be used by api-format providers to
transform objects that cannot be directly emitted into
a structure that can.

If the object is not serializable, an error of type
API-UNSERIALIZABLE-OBJECT is signalled.")

  (variable *api-pages*
    "A map from names to api-pages.

See API-PAGE")

  (function api-page
    "Accessor to the api-page instances.

See API-PAGE
See REMOVE-API-PAGE
See LIST-API-PAGE")

  (function remove-api-page
    "Removes the given api page again if it exists.

See API-PAGE")

  (function list-api-pages
    "Lists all known api page instances.

See API-PAGE")

  (function ensure-api-page
    "Ensures to return the api-page instance for the name.

Accepted types are:
  STRING    --- Looks up the api page by its name
  SYMBOL    --- Coerces to string and tries again
  API-PAGE  --- Returns the argument

If the page cannot be looked up, an error is signalled.

See API-PAGE")

  (type api-page
    "Container for an api endpoint.

See NAME
See HANDLER
See ARGSLIST
See REQUEST-HANDLER")

  (function name
    "Accesses the name of the object.

May be a symbol or a string depending on the object.")

  (function handler
    "Accesses the handler function of the api-page.

The handler-function must have the same lambda-list as
the api-page's ARGSLIST. It must call API-OUTPUT
at some point during its evaluation to emit data.

See API-PAGE")

  (function argslist
    "Accesses the lambda-list of the api-page's handler.

This describes the public interface's arguments.
Arguments are usually received as GET or POST
parameters of the same name as the argument symbol and it
thus only makes sense for the lambda-list to contain 
required and optional arguments.

See API-PAGE")

  (function request-handler
    "Accesses the function to handle a direct request object and transform it into a proper api call.

This function is usually automatically generated.
It should read out the parameters from the request object
and turn them into arguments for a call to the api-page's 
handler function. The function only takes a single argument
namely the request object to handle.

See HANDLER
See API-PAGE")

  (function make-request-handler-function
    "Constructs a standard request handler function for api-pages.

This constructs a lambda expression that extracts the
values from the request's post/get variables and turns
them into arguments for a call to the given function.

See REQUEST-HANDLER")

  (function call-api-request
    "Calls the given api-page with the given request.

This rebinds *REQUEST*.

See REQUEST-HANDLER
See ENSURE-API-PAGE")

  (function call-api
    "Calls the given api-page directly with the supplied arguments.

See HANDLER
See ENSURE-API-PAGE")

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
See API-PAGE
See HANDLER
See REQUEST-HANDLER
See CALL-API
See CALL-API-REQUEST
See EXPAND-OPTIONS"))

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

  (type api-response-empty
    "Error signalled when an api endpoint didn't return any data.

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

Contains a PARAMETER slots that holds the name of the requested
slot.

See REQUEST-ERROR"))

;; config.lisp
(docs:define-docs
  (variable *environment*
    "Holds the currently configured environment name, if any.

See ENVIRONMENT")

  (function environment
    "Accessor to the current environment.

The environment decides the namespace for the configuration
files of Radiance and all modules that use its configuration
system.

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

  (function mconfig-pathname
    "Returns the proper pathname to the module according to the current environment.

The path's base will come from Ubiquitous, the rest
is decided according to the environment and the module.

An environment must have been configured prior to calling
this function.

See UBIQUITOUS:CONFIG-PATHNAME
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

  (function config
    "Shorthand to access the current module's configuration.

This has to be a macro so that the current package can be
captured.

See MCONFIG")

  (function defaulted-mconfig
    "Shorthand to set/retrieve a defaulted value from the module's configuration.

This has to be a macro so that the current package can be
captured.

See DEFAULTED-MCONFIG"))

;; convenience.lisp
(docs:define-docs
  (function ensure-query-form
    "Ensures a proper query form.")

  (function with-model-fields
    "Analogous to WITH-SLOTS but for data-model fields.

See CL:WITH-SLOTS
See DM:FIELD")

  (function with-model
    "Retrieves a model from the database and binds the slots.

If QUERY is NIL, the model is a hull.

See WITH-MODEL-FIELDS
See DM:GET-ONE
See DM:HULL")

  (function with-model-save
    "Same as WITH-MODEL but automatically saves the model at the end.

If the model is initialised as a hull, INSERT is used.

See WITH-MODEL
See DM:SAVE
See DM:INSERT")

  (function do-models
    "Iterates over a bunch of models, binding each in turn.

See WITH-MODEL-FIELDS
See DM:GET")

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
CLAUSE         --- A string designator that names the action"))

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

  (api-page ||
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

Only a single uri-dispatcher will be called, if any.
In order for a dispatcher to be called, the uri must
match the dispatcher's by URI-MATCHES. In the case
where two uri-dispatchers have a matching uri, then
the one with the higher priority will be executed.
This is achived by simply following the order present
in the *uri-priority* vector.

If no matching uri-dispatcher is available, the function
in *URI-FALLBACK* will be called instead.

See URI-MATCHES
See URI-DISPATCHER>
See DISPATCH-FUNCTION
See *URI-PRIORITY*
See *URI-FALLBACK*"))

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

;; handle.lisp
(docs:define-docs
  (variable *debugger*
    "Whether the debugger should be invoked when an error occurs during a request execution.

See HANDLE-CONDITION
See EXECUTE-REQUEST")

  (function handle-condition
    "Responsible for handling a condition during a request execution.

Invokes the debugger if *DEBUGGER* is non-NIL.
Otherwise invokes the SET-DATA restart with the
return-value of calling RENDER-ERROR-PAGE.

See *DEBUGGER*
See EXECUTE-REQUEST
See RENDER-ERROR-PAGE")

  (function render-error-page
    "This function is responsible for rendering an appropriate response for the given condition.

A module is allowed to redefine this function to do as
it sees fit.

See HANDLE-CONDITION")

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
an INTERFACE-IMPLEMENTATION-NOT-SET error is signalled.")

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
See DEFINE-IMPLEMENT-HOOK")

  (function define-implement-hook
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

See *MODULES-DIRECTORY*"))

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

See LIST-OPTIONS
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
See HTP-METHOD
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

  (function headers
    "Accesses the table of headers that the request received or the response should send out.

See REQUEST
See RESPONSE")

  (function post-data
    "Accesses the table of POST-body variables that the request received.

See REQUEST")

  (function get-data
    "Accesses the table of GET variables that the request received.

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
    "Accesses the HTTP content-type to send out.

Defaults to *DEFAULT-CONTENT-TYPE*

See *DEFAULT-CONTENT-TYPE*
See RESPONSE")

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

The content-type, if not explicitly given, is attempted
to be automatically discovered by MIMES:MIME-LOOKUP
and falls back to application/octet-stream.

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

  (function escape-regex-dots-not-in-group
    "Escapes a dot in the string if it is not within a regex capture group.")

  (function define-string-route
    "Defines a route where the URI is analysed by the given regex and translated into the interpolated string representation.

The target string can reference regex capture groups.

See CL-PPCRE:REGEX-REPLACE")

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

  (function cut-get-part
    "Returns the url string without the get part.")

  (function static-file
    "Returns the static file for the given base.

The base will usually be your local module and thus
this will use the static folder within its source
directory.")

  (function template
    "Returns the template file for the given base.

The base will usually be your local module and thus
this will use the template folder within its source
directory.")

  (function perm
    "Macro to encompass a permission.

You should use this wherever you reference a permission.
Using this will ensure that the permission is registered
with your module and thus inspectable from the outside.")

  (function copy-hash-table
    "Copies the given hash-table as accurately as possible."))

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
    "Returns a URL representation of the URI assuming an HTTP context.

See REPRESENT-URI"))
