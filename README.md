## About Radiance
Radiance is a web application environment, which is sort of like a web framework, but more general, more flexible. It should let you write personal websites and generally deployable applications easily and in such a way that they can be used on practically any setup without having to undergo special adaptations.

Radiance is currently lacking in documentation to be advocated as a ready-to-use framework. Please be patient until the remaining [issues](https://github.com/Shirakumo/radiance/issues) have been cleaned up. It is however already being used in a production setting for [TymoonNET](https://blog.tymoon.eu/). See the remaining [Shirakumo projects](https://github.com/Shirakumo) for examples of applications built using Radiance.

## Getting It
Radiance and associated modules and applications are distributed via Quicklisp in a separate dist. To install Radiance, do:

    (ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
    (ql:quickload :radiance)

From there on out you should be able to load and use any kind of Radiance module like [Purplish](https://github.com/Shirakumo/purplish) directly via Quicklisp's `quickload`.

## Using It
### A Simple Example
The most basic thing you most likely want to do is serve some kind of HTML. So let's work towards that and gradually extend it. Before we can begin, we need to start up Radiance.

```common-lisp
(ql:quickload :radiance)
(radiance:startup)
```

If this is your first time setting up Radiance, you'll get a note about it using the `r-welcome` module. It should also give you a link that you can open in your browser to see a little greeting page. For now we'll just want to put up our own little page alongside it. We'll see about setting up a proper environment later.

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

Refresh the page, and voilà, now it's got some pizzas to it too. You'll probably want an explanation for the whole `uri-to-url` business. Explaining it in full is handled by the sections following this one, but the gist of it is that it ensures that the link to the static file is properly resolved under any setup.

### Radiance Concepts
TBD

### Request Lifecycle
TBD

### Module Management
TBD

### Interfaces & You
TBD

## Also See

* [modularize](https://shinmera.github.io/modularize) For the primary package metadata system
* [modularize-interfaces](https://shinmera.github.io/modularize-interfaces) For the interface and implementations extensions
* [modularize-hooks](https://shinmera.github.io/modularize-hooks) For the hooks and triggers mechanisms
* [ubiquitous](https://shinmera.github.io/ubiquitous) For configuration management
