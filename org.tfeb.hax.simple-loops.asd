;;;; Module org.tfeb.hax.simple-loops of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.simple-loops"
  :description
  "A subsystem of the TFEB hax"
  :version
  "5.0.0"
  :author
  "Tim Bradshaw"
  :license
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-hax"
  :depends-on
  ("org.tfeb.hax.collecting"
   "org.tfeb.hax.iterate"
   "org.tfeb.hax.utilities")
  :components
  ((:file "simple-loops")))
