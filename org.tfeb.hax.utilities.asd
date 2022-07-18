;;;; Module org.tfeb.hax.utilities of org.tfeb.hax
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.hax.utilities"
  :description
  "A subsystem of the TFEB hax"
  :version
  "5.0.1"
  :author
  "Tim Bradshaw"
  :license
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-hax"
  :depends-on
  ("org.tfeb.hax.collecting" "org.tfeb.hax.iterate")
  :components
  ((:file "utilities")))
