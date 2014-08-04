(asdf:defsystem #:i-verbose
  :defsystem-depends-on (:radiance)
  :class "radiance:module"
  :components ((:file "i-verbose"))
  :depends-on (:verbose))
