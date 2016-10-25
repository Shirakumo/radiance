<img alt="logo" src="data/static/radiance.png" width="512" align="center"/>

About Radiance
--------------
An explanation about what exactly Radiance is will be here as soon as I figure out how to explain it quickly enough. In the most basic of words, it is a very featureful web-framework written in Common Lisp.

Radiance is currently lacking in completeness, soundness, and specification to be advocated as a ready-to-use framework. Please be patient until the remaining [issues](https://github.com/Shirakumo/radiance/issues) have been cleaned up. It is however already being used in a production setting for [TymoonNET](https://blog.tymoon.eu/). See the remaining [Shirakumo projects](https://github.com/Shirakumo) for examples of applications built using Radiance.

Getting It
----------
Radiance and associated modules and applications are distributed via Quicklisp in a separate dist. To install Radiance, do:

    (ql-dist:install-dist "http://dist.tymoon.eu/shirakumo.txt")
    (ql:quickload :radiance)

From there on out you should be able to load and use any kind of Radiance module like [Purplish](https://github.com/Shirakumo/purplish) directly via Quicklisp's `quickload`.

Using It
--------
This section too will be done once all issues have been cleaned up. For now you can have a preview peek at how things are (most likely) going to be, as well as read information on the progress of the project, in the associated [radiance articles](http://blog.tymoon.eu/tagged/radiance).
