## About Radiance
Radiance is a web application environment, which is sort of like a web framework, but more general, more flexible. It should let you write personal websites and generally deployable applications easily and in such a way that they can be used on practically any setup without having to undergo special adaptations.

Radiance is currently lacking in completeness, soundness, and specification to be advocated as a ready-to-use framework. Please be patient until the remaining [issues](https://github.com/Shirakumo/radiance/issues) have been cleaned up. It is however already being used in a production setting for [TymoonNET](https://blog.tymoon.eu/). See the remaining [Shirakumo projects](https://github.com/Shirakumo) for examples of applications built using Radiance.

## Getting It
Radiance and associated modules and applications are distributed via Quicklisp in a separate dist. To install Radiance, do:

    (ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
    (ql:quickload :radiance)

From there on out you should be able to load and use any kind of Radiance module like [Purplish](https://github.com/Shirakumo/purplish) directly via Quicklisp's `quickload`.

## Using It
### A Simple Example
TBD

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
