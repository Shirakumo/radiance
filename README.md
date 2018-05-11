## Radianceについて

Radianceは、Webアプリケーションの開発環境です。

Webフレームワークのようなものですが、より汎用的で、変更も容易です。 

特別な変更を加える必要なく、個人的なWebサイトやWebアプリを、簡単に書くことができます。

## Getting It
Radiance and associated modules and applications are distributed via Quicklisp in a separate dist. To install Radiance, do:

    (ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
    (ql:quickload :radiance)

From there on out you should be able to load and use any kind of Radiance module like [Purplish](https://github.com/Shirakumo/purplish) directly via Quicklisp's `quickload`.

## A Lengthy and In-Depth Example
You can find a tutorial that introduces Radiance and most of the important concepts, and explores how to write a web application in general, [here](https://github.com/Shirakumo/radiance-tutorial/blob/master/Part%200.md). It should give you a good feeling for how to go about things, and give you pointers about where to look if you need a particular feature. In the last part it'll also go into the actual setup and deployment of a Radiance installation on a production server.

## A Simple Example
The most basic thing you most likely want to do is serve some kind of HTML. So let's work towards that and gradually extend it. Before we can begin, we need to start up Radiance.

```common-lisp
(ql:quickload :radiance)
(radiance:startup)
```

If this is your first time setting up Radiance, you'll get a note about it using the `r-welcome` module. It should also give you a link that you can open in your browser to see a little greeting page. For now we'll just want to put up our own little page alongside it.

```common-lisp
(in-package :rad-user)

(define-page example "/example" ()
  (setf (content-type *response*) "text/plain") 
  "Hi!")
```

Visiting [localhost:8080/example](http://localhost:8080/example) should now just show "Hi". Rather boring indeed. So let's spit out some HTML instead. For now, we'll use [cl-who](http://weitz.de/cl-who/) since it is very simple.

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

A recompile and refresh later and we have some font styling going on. Next we'll probably want to add a CSS file to it to style it properly. We could serve the CSS using another page as well, but that isn't the best way to go about things in the long term.

Let's instead look at how to create a module, which will allow us to organise things in a more orderly fashion. You can create the files for a module manually, but for now we'll settle with an automatically generated skeleton that Radiance can provide you with.

```common-lisp
(create-module "example")
```

It should return you a path on which the module resides. It should contain an ASDF system, a main lisp file, and two folders, `static` and `template`. Surprisingly enough, the `static` folder is where statically served files go, and `template` is for template documents, if you happen to use a template system.

Let's open up the `example.lisp` and carry over our example page from it.

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page"))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

Pages are identified by a name symbol. Since we now have our own module, and thus our own package, the example symbol above won't be the same as the one we've used before. We'll just have to remove the page in the `rad-user` package to avoid the clash.

```common-lisp
(remove-page 'rad-user::example)
```

Next let's create a simple CSS file to spruce things up a little. The file will be `example.css` placed in the `static` folder. Here's a sample CSS if you don't want to write your own.

```CSS
body{
    font-family: sans-serif;
    font-size: 12pt;
    background: #EEE;
}

header{
    text-align: center;
}

main{
    width: 800px;
    margin: 0 auto 0 auto;
    background: #FFF;
    padding: 10px;
    border: 1px solid #BBB;
    border-radius: 5px;
}
```

Next we need to modify our HTML to actually link to the style sheet. In order to get the address to the stylesheet we'll have to make use of Radiance's routing system. Don't worry though, it's not much of a hassle.

```common-lisp
(define-page example "/example" ()
  (cl-who:with-html-output-to-string (o)
    (cl-who:htm
     (:html
      (:head (:title "Example Page")
             (:link :rel "stylesheet" :type "text/css" 
                    :href (uri-to-url "/static/example/example.css" :representation :external)))
      (:body (:header (:h1 "Couldn't Be Simpler."))
             (:main (:p "Trust me on this one.")))))))
```

Refresh the page, and voilà, now it's got some pizzazz to it too. You'll probably want an explanation for the whole `uri-to-url` business. Explaining it in full is handled by the sections following this one, but the gist of it is that it ensures that the link to the static file is properly resolved under any setup.

## 1. Radiance Concepts & Parts
### 1.1 URI
One of the most central concepts in Radiance is that of a URI. A URI is an object that consists of a list of domains, an optional port number, and a path (see `uri`). It is essentially a stripped down version of a general URI, and as such doesn't include a schema, query, or fragment part. Another important difference is that the `domains`  URIs are used at several points throughout the framework, both to capture locations and to handle dispatch matching.

Note that URIs are mutable. This is important for performance, as URI modifications have to happen in several parts that lie on the critical path. However, in the usual case it is not expected that URIs are modified outside of a few select functions. Modifying a URI's parts in unexpected ways may lead to strange behaviour.

URIs have a unique string representation and can be serialised to string and parsed back into a full URI object again. URIs can also be dumped to FASL files as literals, so emitting them from macros is fine. The syntax for a URI is as follows:

```BNF
URI     ::= DOMAINS? (':' PORT)? '/' PATH?
DOMAINS ::= DOMAIN ('.' DOMAIN)*
DOMAIN  ::= ('a'..'Z' | '0'..'9' | '-')
PORT    ::= ('0'..'9'){1, 5}
PATH    ::= .*
```

You can use `uri-to-url` to turn a URI into a concrete URL. Reversal, encoding, and proper formatting of all the parts is handled for you automatically there.

See `uri`, `domains`, `port`, `path`, `matcher`, `uri-string`, `make-uri`, `make-url`, `ensure-uri`, `copy-uri`, `parse-uri`, `uri<`, `uri>`, `uri=`, `uri-matches`, `merge-uris`, `represent-uri`, `uri-to-url`.

### 1.2 Request and Response
In order to encapsulate the data that is sent to  and from, we have the idea of a Request (`request`) and Response (`response`) object. The Request object holds the URI that represents to which location the request goes, and all the data contained in the HTTP payload like post, get, header, and cookie variables. The Response object holds the return-code, headers, cookies, and the actual body data.

During the processing of a request, these two objects must always be present and bound to the `*request*` and `*response*` variables. They encapsulate a lot of very vital information that is necessary to generate dynamic pages. Additionally, the Request contains an opaque `data` table in which you can store arbitrary data. This is useful when you need to exchange pieces of information between individual parts of the system that may be reached during the request execution.

Requests don't necessarily have to come from the HTTP server. In order to test things you can also construct a request yourself and send it out programmatically. Whatever the case, the primary interface to dispatch a request is called `request`. This will construct a Request and Response object for you and appropriately handle the URI. If you want to do that yourself and *really* just send out a complete Request object, you can use `execute-request`.

For the actual handling of a request, see dispatchers, pages, and API endpoints.

See `*request*`, `*response*`, `*default-external-format*`, `*default-content-type*  `, `request`, `uri`, `http-method`, `headers`, `post-data`, `get-data`, `cookies`, `user-agent`, `referer`, `domain`, `remote`, `data`, `issue-time`, `response`, `data`, `return-code`, `content-type`, `external-format`, `headers`, `cookies`, `cookie`, `name`, `value`, `domain`, `path`, `expires`, `http-only`, `secure`, `cookie-header`, `cookie`, `get-var`, `post-var`, `post/get`, `header`, `file`, `redirect`, `serve-file`, `request-run-time`, `*debugger*`, `handle-condition`, `render-error-page`, `execute-request`, `set-data`, `request`

### 1.3 Route
Before a Request can be dispatched on, it goes through something called the routing system. Unlike in other frameworks, where 'routes' designate what handles a request, in Radiance a Route (`route`) is a form of URI translator. This part of the system is what's responsible for creating and upholding two "universes", an internal and an external one.

The internal universe is the one actual web applications live in. The external universe is the one the HTTP server and a user of the website lives in. This distinction is necessary in order to allow you to, one one hand, write web applications without having to worry about what a potential setup on a server might look like at some point. You don't have to worry about what kind of domain, port, path setup may be necessary to run your application. On the other hand, it allows you, as a webadmin, to customise and run the system to your exact desires without fear of breaking things.

This all is facilitated by routes, of which there are two kinds: mapping, and reversal routes. Mapping routes are responsible for turning a URI from the external universe into one of the internal universe. Usually this involves cutting away the top-level domain and perhaps doing a mapping of subdomains. Reversal routes do the opposite-- they go from the internal universe to the external. This is necessary in order to make links in your served pages refer to resources that are actually accessible from the outside. Usually this involves reversing the subdomain mapping and adding the top-level domain again.

Routes can perform arbitrary work. At the most basic level, they are merely functions that modify a URI in some fashion. This allows you to create a very flexible system that should be powerful enough to accommodate to all of your needs as an administrator. As an application writer, you just need to make sure to use `external-uri` or `uri-to-url` on all of the links that you put into your pages.

See `route`, `name`, `direction`, `priority`, `translator`, `route`, `remove-route`, `list-routes`, `define-route`, `define-matching-route`, `define-target-route`, `define-string-route`, `internal-uri`, `external-uri`

### 1.4 URI Dispatcher
Finally we come to the part that actually generates content for a request. URI dispatchers are a subclass of URI that also carry a name, a function, and a priority. The live in a priority-sorted list, which is processed whenever a request arrives. The Request's URI is matched against each dispatcher. The function of the first dispatcher that matches is then executed.

And that's it. The dispatcher's function is responsible for setting the necessary values in the Response object to deliver the page content. In order to do this it can either directly set the `data` field of the Response object, or you can return an appropriate value from the function. Radiance only accepts four types of values: `stream`, `pathname`, `string`, and `(array (unsigned-byte 8))`.

If a URI dispatcher does not have an explicit priority number, its priority over others is determined by the specificity of the URI. See the URI sorting function `uri>` for an explanation on how exactly this is calculated.

See `uri-dispatcher`, `name`, `dispatch-function`, `priority`, `uri-dispatcher`, `remove-uri-dispatcher`, `list-uri-dispatchers`, `uri-dispatcher>`, `define-uri-dispatcher`, `dispatch`

### 1.5 Page
Pages are what you will likely use to define your actual content serving functions. However, a page is just a uri-dispatcher with some extra functionality in the definition macro that makes things easier on you. Most notably are the extensible options, for which you can find an explanation below.

There are a couple of default pages set up by Radiance itself. First there's the `favicon` and `robots` pages, which simply serve the respective files from Radiance's `static/` directory. You'll probably want to either provide your own pages for that or update the files on your production server.

Then there's the `static` page, which is responsible for serving static contents for all web applications and modules. It should be active on any domain and always on the path `/static/...` where `...` must have a form where the first directory is the name of a module, and the rest is a path within that module's `static/` directory. This allows you to always be able to refer to static files like CSS, JS, and images through a common path.

Finally there's the `api` page, which is responsible for handling the dispatch of API endpoints, which are explained in the following section. The page acts similarly to the static one by capturing the `/api/...` path on all domains.

See `page`, `remove-page`, `define-page`

### 1.6 API Endpoint
Radiance provides integrated support for REST API definition. This is not just a tacked-on feature, but rather because most modern applications want to provide an API of some kind, and because Radiance advises a certain way of writing your applications that necessarily involves API endpoints.

Conceptually, API endpoints are functions that are callable through a browser request. Their response is then serialised to a format that is readable by the requester, whatever that may be. Important to remember however is that API endpoints should be usable by both users and programs. Radiance encourages this because usually any kind of action that can be performed programmatically through an API will also have to be performed by the user in some way. In order to avoid duplication, the two can be conflated.

As such, usually any kind of data modification action should be provided through an API endpoint that reacts slightly differently depending on whether a user or an application requests it. In the case of a user, it should usually redirect back to an appropriate page, and in the case of an application it should provide a data payload in a readable format.

The first part of all of this is the API format system, which is responsible for serialising data to some specified format. By default only an S-expression based format is supplied, but a contrib to get JSON output can easily be loaded.

The second part is the specification of the `browser` POST/GET parameter. If that parameter contains the exact string `"true"`, then the API request is treated as coming from a user, and thus a redirect rather than a data payload should be outputted.

Your application should make use of those things in order to provide a properly integrated api. Now, an actual endpoint definition is composed of a name, a raw function, a lambda-list describing the arguments of the function, and a request parsing function. Typically for your arguments, only required and optional arguments make sense. After all, an HTTP request only has "keyword arguments" that it can provide, and those can either be present or missing.

The name of an API endpoint also serves as the identifier that tells you where you can reach it. API endpoints live on the `/api/` path, followed by the name of the endpoint. As such, you are responsible for prefixing your endpoints with the name of your module or application in order to avoid accidentally tripping over other endpoints. This is unlike in uri dispatchers, because API endpoints have to match exactly and don't allow any ambiguity or processing of the path. Thus every endpoint must have a unique path, which can also immediately serve as the name.

The raw function is the function that the API provides an interface for. It is responsible for performing the requested action and returning the appropriate data as described above. For returning formatted API data, see `api-output`. For redirecting in the case of a browser request, see `redirect`.

Finally, the request parsing function is responsible for taking a Request object, extracting the arguments the function needs from it, and finally calling that function with the appropriate arguments-- if possible. The parsing function may signal an `api-argument-missing` error if a required argument is missing. Superfluous arguments should be ignored.

You can also programmatically call an API endpoint using `call-api`, or simulate a Request call with `call-api-request`, without having to go through the whole URI dispatch mechanism.

Similarly to pages, API endpoint definitions also accept extensible options that make definition simpler. See the following section for an explanation of options.

See `api`, `*default-api-format*`, `*serialize-fallback*`, `api-format`, `remove-api-format`, `list-api-formats`, `define-api-format`, `api-output`, `api-serialize`, `api-endpoint`, `remove-api-endpoint`, `list-api-endpoints`, `api-endpoint`, `name`, `handler`, `argslist`, `request-handler`, `call-api-request`, `call-api`, `define-api`

### 1.7 Options
Options are a way of providing an extensible definition macro. This is useful when a framework provides a common way of defining something, but other parts may want to provide extensions to that in order to make common operations shorter. For example, a common task is to restrict a page or API endpoint to people who have the required access credentials.

In order to facilitate this, Radiance provides a rather generic options mechanism. Options are divided up by an option type that designates to which definition macro the option belongs. Radiance provides the `api` and `page` option types out of the box.

Each option has a keyword for a name and an expander function that must accept a number of arguments, depending on the option type. Always provided as arguments are the name of the thing being defined, the list of body forms of the definition, and a final, optional, value that was provided to the option in the options list, if it was mentioned at all. This expansion function is then responsible for transforming the body forms of the definition macro in some way. It can also emit a second form that is placed outside of the definition itself, in order to allow setting up the environment in some manner.

See `option`, `option-type`, `name`, `expander`, `option`, `remove-option`, `list-options`, `define-option`, `expand-options`

### 1.8 Module
The concept of a module is essential to Radiance. It serves as the representation of a "part" of the whole. On a technical level, a module is a package that has special metadata attached to it. It is provided by the `modularize` system and is used to facilitate hooks and triggers, interfaces, and the tracking of a few other pieces of information.

What this means for you is that instead of a standard `defpackage` you should use a `define-module` form to define your primary package. The syntax is the same as `defpackage`, but includes some extra options like `:domain`, which allows you to specify the primary domain on which this module should operate (if any).

The module system also allows the tying of an ASDF system to a module. If that is done, then the ASDF system becomes a "virtual module". In order to do this, you must add three options to your system definition:

```commonlisp
:defsystem-depends-on (:radiance)
:class "radiance:virtual-module"
:module-name "MY-MODULE"
```

This allows Radiance to identify and associate ASDF system information to your module. For automated creation of the necessary system and module definitions for a new module, see `create-module`.

See `virtual-module`, `virtual-module-name`, `define-module`, `define-module-extension`, `delete-module`, `module`, `module-p`, `module-storage`, `module-storage-remove`, `module-identifier`, `module-name`, `current-module`, `module-domain`, `module-permissions`, `module-dependencies`, `module-required-interfaces`, `module-required-systems`, `module-pages`, `module-api-endpoints`, `describe-module`, `find-modules-directory`, `*modules-directory*`, `create-module`

### 1.9 Hooks
One of the mechanisms that Radiance provides to allow integrating modules into each other is hooks. Hooks allow you to run an arbitrary function in response to some kind of event. For example, a forum software might set up a hook that is triggered whenever a new post is created. An extension could then define a trigger on that hook that performs additional tasks.

A hook can have an arbitrary number of triggers defined on it, but you should ensure that a trigger does not take too long, as triggering a hook is a blocking operation that won't finish until all of the triggers have completed. As such, a long-running trigger operation might delay a request response for too long.

Sometimes hooks should function more like switches, where they can be "on" for a long time, until they're turned "off" again later. If new triggers are defined during that time, they should be called automatically. This is what the `define-hook-switch` facilitates. It produces two hooks. Once the first one has been triggered, any trigger that is defined on it later is called automatically until the second hook is triggered. This allows triggers on hooks like `server-start` to function properly even if the trigger is only defined after the server has already been started.

See `list-hooks`, `define-hook`, `remove-hook`, `define-trigger`, `remove-trigger`, `trigger`, `define-hook-switch`

### 1.10 Interface
In order to avoid becoming monolithic, and in order to allow extensible backends, Radiance includes an interface system. In the most general sense, an interface provides a promise as to how some functions, macros, variables, etc. should work, but does not actually implement them. The actual functionality that makes everything that the interface outlines work is pushed off to an implementation. This allows users to code against an interface and make use of its provided functionality, without tying themselves to any particular backend.

For a concrete example, let's say there's an interface for a database. This is sensible, since there are many different kinds of databases, that all offer many differing ways of interaction, but still all also offer some very common operations: storing data, retrieving data, and modifying the data. Thus we create an interface that offers these common operations. It is then up to an implementation for a specific kind of database to make the actual operations work. As an application writer, you can then make use of the database interface, and with it, make your application automatically work with lots of different databases.

Aside from giving application writers an advantage, the decoupling that the interfaces provide also mean that a system administrator can write their own implementation with relative ease, should their particular requirements not be met by existing implementations. Thanks to the opaqueness of the interfaces, an implementation can both provide a bridge to something that runs in the lisp process, and something that is completely external. This leaves a lot of choice open for the administrator of a production system to allow them to pick exactly what they need.

In practise, interfaces are special kinds of modules, and thus special kinds of packages. As part of their definition, they include a series of definitions for other bindings like functions, variables, etc. Since it is a package, as a user you can use the interface's components just like you would use anything else in any other package. There is no difference. As an implementation writer, you then simply redefine all the definitions that the interface outlines.

In order to actually load a module that makes use of an interface, an implementation for the interface has to be loaded beforehand. Otherwise, macros could not work properly. Thus, in order to allow depending on interfaces in your ASDF system definition without having to refer to a specific implementation, Radiance provides an ASDF extension. This extension makes it possible to add a list like `(:interface :foo)` to your `:depends-on` list. Radiance will then resolve the interface to a concrete implementation thereof when the module is loaded.

Radiance provides a bunch of standard interfaces. Each of those interfaces has at least one standard implementation provided by [radiance-contribs](https://shirakumo.org/projects/radiance-contribs). The interfaces are:

* `admin`  
  Provides an extensible administration site.
* `auth`  
  Handles everything about authentication and login.
* `ban`  
  Allows banning users from the site by their IP address.
* `cache`  
  Provides a very simple caching interface.
* `database`  
  A flexible database interface that allows both object-stores and relational databases as backends.
* `logger`  
  A simple logging interface to allow printing debug and information messages.
* `mail`  
  Minimal interface to send emails with.
* `profile`  
  Provides an extensible user profile site and user fields.
* `rate`  
  Allows rate limitation for access to certain resources.
* `server`  
  The interface that bridges to a server to connect Radiance with an external universe.
* `session`  
  Ensures persistent sessions for users to allow tracking them.
* `user`  
  Provides user accounts and permissions.
  
The interfaces are described in-depth below.

See `interface`, `interface-p`, `implementation`, `implements`, `reset-interface`, `define-interface-extension`, `find-implementation`, `load-implementation`, `define-interface`, `define-implement-trigger`

### 1.11 Environment
In order to permit running multiple instances of Radiance with different setups on the same machine, Radiance provides what it calls an Environment system. The Environment is basically the set of configuration files for Radiance itself and all of the loaded modules. The Radiance configuration also includes the mapping of interface to chosen implementation and thus decides what should be picked if an interface is requested.

The particular environment that is used is chosen at the latest when `startup` is called, and the earliest when a module is loaded. In the latter case, interactive restarts are provided to allow you to pick an environment. This is necessary, as otherwise Radiance won't be able to resolve the interface mapping.

As part of the environment system, Radiance provides you with a configuration system that you can --and probably should-- use for your application. It ensures that the settings are properly multiplexed for each environment, and that the settings are always persistent. It also uses a human-readable storage format, such that the files can be read and modified without requiring any special tools.

See [ubiquitous](https://shinmera.github.io/ubiquitous) for the actual handling and use-instructions of the configuration storage. Just note that instead of the `value` functions, Radiance provides `config` functions.

See `environment-change`, `environment`, `check-environment`, `mconfig-pathname`, `mconfig-storage`, `mconfig`, `defaulted-mconfig`, `config`, `defaulted-config`

### 1.12 Instance Management
Finally, Radiance provides a standard startup and shutdown sequence that should ensure things are properly setup and readied, and afterwards cleaned up nicely again. A large part of that sequence is just ensuring that certain hooks are called in the proper order and at the appropriate times.

While you can start a server manually by using the appropriate interface function, you should not expect applications to run properly if you do it that way. Many of them will expect certain hooks to be called in order to work properly. This is why you should always, unless you exactly know what you're doing, use `startup` and `shutdown` to manage a Radiance instance. The documentation of the two functions should explain exactly which hooks are triggered and in which order. An implementation may provide additional, unspecified definitions on symbols in the interface package, as long as said symbols are not exported.

See `*startup-time*`, `uptime`, `server-start`, `server-ready`, `server-stop`, `server-shutdown`, `startup`, `startup-done`, `shutdown`, `shutdown-done`, `started-p`

## 2. Standard Interfaces
These interfaces are distributed with Radiance and are part of the core package. Libraries may provide for additional interfaces, however. For implementations of standard interfaces, the following relaxations of interface definition constraints are allowed:

The lambda-lists that contain `&key` arguments can be extended by further, implementation-dependant keyword arguments. Lambda-lists that contain `&optional` but no `&key` or `&rest` may be extended by further optional arguments. Lambda-lists that contain only required arguments may be extended by further optional or keyword arguments.

### 2.1 admin
This interface provides for an administration page. It should be used for any kind of user-configurable settings, or system information display. Note that despite being called "administration", this is not intended solely for administrators of the system. The page should be usable for any user.

The administration page is required to be able to display a categorised menu, and a panel. The panels are provided by other modules and can be added through `admin:define-panel`. Panels that give access to sensitive operations should be appropriately restricted through the `:access` option and a non-default permission. See the user interface for an explanation on the permissions.

In order to link to the administration page, or a specific panel on it, use the `page` resource type.

See `admin:list-panels`, `admin:remove-panel`, `admin:define-panel`, `admin:panel`

### 2.2 auth
The authentication interface is responsible for tying a user to a request. For this reason it must provide some manner by which a user can authenticate themselves against the system. How this is done exactly is up to the implementation. The implementation must however provide a page on which the authentication process is initiated. You can get a URI to it through the `page` resource and passing `"login"` as argument.

You can test for the user currently tied to the request by `auth:current`. This may also return `NIL`, in which case the user should be interpreted as being `"anonymous"`. See the user interface for more information.

See `auth:*login-timeout*`, `auth:page`, `auth:current`, `auth:associate`

### 2.3 ban
This interface provides for IP-banning. It must prevent any client connecting through a banned IP from seeing the content of the actual page they're requesting. Bans can be lifted manually or automatically after a timeout. The implementation may or may not exert additional effort to track users across IPs.

See `ban:jail`, `ban:list`, `ban:jail-time`, `ban:release`

### 2.4 cache
The cache interface provides for a generic caching mechanism with a customisable invalidation test. You can explicitly renew the cache by `cache:renew`. To define a cached block, simply use `cache:with-cache`, which will cause the cached value of the body to be returned if the test form evaluates to true.

The exact manner by which the cached value is stored is up to the implementation and `cache:get` or `cache:with-cache` may coerce the cached value to a string or byte array. The implementation may support any number of types of values to cache, but must in the very least support strings and byte arrays.

The name for a cached value must be a symbol whose name and package name do not contain any of the following characters: `<>:"/\|?*.` The variant for a cached value must be an object that can be discriminated by its printed (as by `princ`) representation. The same character constraints as for the name apply.

See `cache:get`, `cache:renew`, `cache:with-cache`

### 2.5 database
This interface provides you with a data persistence layer, usually called a database. This does not have to be a relational database, but may be one. In order to preserve implementation variance, only basic database operations are supported (no joins, triggers, etc). Data types are also restricted to integers, floats, and strings. Despite these constraints, the database interface is sufficiently useful for most applications.

Note that particular terminology is used to distance from traditional RDBMS terms: a schema is called a "structure". A table is called a "collection". A row is called a "record".

Performing database operations before the database is connected results in undefined behaviour. Thus, you should put your collection creation forms (`db:create`) within a trigger on `db:connected`. Radiance ensures that the database is connected while Radiance is running, so using the database interface in any page, api, or uri dispatcher definitions is completely fine.

The functions for actually performing data storage are, intuitively enough, called `db:insert`, `db:remove`, `db:update`, `db:select`, and `db:iterate`. The behaviour thereof should be pretty much what you'd expect. See the respective docstrings for a close inspection. Also see the docstring of `db:create` for a lengthy explanation on how to create a collection and what kind of restrictions are imposed.

The database must ensure that once a data manipulation operation has completed, the changes caused by it will be persisted across a restart of Radiance, the lisp image, or the machine, even in the case of an unforeseen crash.

See `database:condition`, `database:connection-failed`, `database:connection-already-open`, `database:collection-condition`, `database:invalid-collection`, `database:collection-already-exists`, `database:invalid-field`, `database:id`, `database:ensure-id`, `database:connect`, `database:disconnect`, `database:connected-p`, `database:collections`, `database:collection-exists-p`, `database:create`, `database:structure`, `database:empty`, `database:drop`, `database:iterate`, `database:select`, `database:count`, `database:insert`, `database:remove`, `database:update`, `database:with-transaction`, `database:query`, `database:connected`, `database:disconnected`

### 2.6 logger
This interface provides primitive logging functions so that you can log messages about relevant happenings in the system. The actual configuration of what gets logged where and how is up to the implementation and the administrator of the system.

See `logger:log`, `logger:trace`, `logger:debug`, `logger:info`, `logger:warn`, `logger:error`, `logger:severe`, `logger:fatal`

### 2.7 Mail
With the mail interface you get a very minimal facility to send emails. A variety of components might need email access, in order to reach users outside of the website itself. The configuration of the way the emails are sent --remote server, local sendmail, etc.-- is implementation dependant.

The `mail:send` hook provided by the interface allows you to react to outgoing emails before they are sent.

See `mail:send`

### 2.8 profile
The profile interface provides extensions to the user interface that are commonly used in applications that want users to have some kind of presence. As part of this, the interface must provide for a page on which a user's "profile" can be displayed. The profile must show panels of some kind. The panels are provided by other modules and can be added by `profile:define-panel`.

You can get a URI pointing to the profile page of a user through the `page` resource type.

The interface also provides access to an "avatar image" to visually identify the user (`profile:avatar`), a customisable name that the user can change (`profile:name`), and field types to what kind of data is contained in a user's field and whether it should be public information or not (`profile:fields` `profile:add-field` `profile:remove-field`).

See `profile:page`, `profile:avatar`, `profile:name`, `profile:fields`, `profile:add-field`, `profile:remove-field`, `profile:list-panels`, `profile:remove-panel`, `profile:define-panel`, `profile:panel`

### 2.9 rate
This interface provides for a rate limitation mechanism to prevent spamming or overly eager access to potentially sensitive or costly resources. This happens in two steps. First, the behaviour of the rate limitation is defined for a particular resource by `rate:define-limit`. Then the resource is protected through the `rate:with-limitation` macro. If the access to the block by a certain user is too frequent, the block is not called, and the code in the limit definition is evaluated instead.

Note that rate limitation is per-client, -user, or -session depending on the implementation, but certainly not global.

See `rate:define-limit`, `rate:left`, `rate:with-limitation`

### 2.10 server
This and the logger interface are the only interfaces Radiance requires an implementation for in order to start. It is responsible for accepting and replying to HTTP requests in some manner. The implementation must accept requests and relay them to the Radiance `request` function, and then relay the returned `response` back to the requester.

Note that the actual arguments that specify the listener behaviour are implementation-dependant, as is configuration thereof. However, if applicable, the implementation must provide for a standard listener that is accessible on `localhost` on the port configured in `(mconfig :radiance :port)` and is started when `radiance:startup` is called.

See `server:start`, `server:stop`, `server:listeners`, `server:started`, `server:stopped`

### 2.11 session
The session interface provides for tracking a client over the course of multiple requests. It however cannot guarantee to track clients perfectly, as they may do several things in order to cloak or mask themselves or falsify information. Still, for most users, the session tracking should work fine enough.

The session interface is usually used by other interfaces or lower-lying libraries in order to provide persistence of information such as user authentication.

See `session:*default-timeout*`, `session:session`, `session:=`, `session:start`, `session:get`, `session:list`, `session:id`, `session:field`, `session:timeout`, `session:end`, `session:active-p`, `session:create`

### 2.12 user
This interface provides for persistent user objects and a permissions system. It does not take care of authentication, identification, tracking, or anything of the sort. It merely provides a user object upon which to build and with which permissions can be managed.

See `user:user` for a description of permissions and their behaviour.

See `user:condition`, `user:not-found`, `user:user`, `user:=`, `user:list`, `user:get`, `user:username`, `user:fields`, `user:field`, `user:remove-field`, `user:remove`, `user:check`, `user:grant`, `user:revoke`, `user:add-default-permissions`, `user:create`, `user:remove`, `user:action`, `user:ready`, `user:unready`

## Also See

* [modularize](https://shinmera.github.io/modularize) For the primary package metadata system
* [modularize-interfaces](https://shinmera.github.io/modularize-interfaces) For the interface and implementations extensions
* [modularize-hooks](https://shinmera.github.io/modularize-hooks) For the hooks and triggers mechanisms
* [ubiquitous](https://shinmera.github.io/ubiquitous) For configuration management
* [radiance-contribs](https://shirakumo.org/projects/radiance-contribs) Default interface implementations and other convenience modules for Radiance
