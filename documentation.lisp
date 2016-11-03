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
  (option (page with-trigger)
    "Adds a hook that is automatically triggered when the page is called.

Unless otherwise specified, the hook is named the same as the page.")

  (option (page uri-groups)
    "Allows capturing the regex groups in the URI's path as variables.

See CL-PPCRE:REGISTER-GROUPS-BIND")

  (function transform-access-body
    "Transforms the body to be protected from access by the permission branch.")

  (option (page access)
    #1="Ensures that the page is only accessible if the user requesting it has the appropriate permission branch.")
  
  (option (api access)
    #1#)

  (option (admin:panel access)
    #1#)

  (option (profile:panel access)
    #1#)

  (api-page ||
    "Fallback api endpoint that signals an API-CALL-NOT-FOUND error.")

  (page favicon
    "Standard page for the favicon.ico root image.

See DATA-FILE")

  (page robots
    "Standard page for the robots.txt root file.

See DATA-FILE")

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
    "Accessor to the priority of the uri dispatcher, which may be NIL or an INTEGER.

See URI-DISPATCHER
See DISPATCH")

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

;; init.lisp
(docs:define-docs
  )

;; interface-components.lisp
(docs:define-docs
  )

;; interfaces.lisp
(docs:define-docs
  )

;; modules.lisp
(docs:define-docs
  )

;; options.lisp
(docs:define-docs
  )

;; page.lisp
(docs:define-docs
  )

;; pattern.lisp
(docs:define-docs
  )

;; request.lisp
(docs:define-docs
  )

;; resource.lisp
(docs:define-docs
  )

;; routing.lisp
(docs:define-docs
  )

;; standard-interfaces.lisp
(docs:define-docs
  )

;; toolkit.lisp
(docs:define-docs
  )

;; uri.lisp
(docs:define-docs
  )

