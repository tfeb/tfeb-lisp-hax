;;;; Module org.tfeb.hax.iterate of org.tfeb.hax
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.hax.iterate"
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
  :components
  ((:file "iterate")))
