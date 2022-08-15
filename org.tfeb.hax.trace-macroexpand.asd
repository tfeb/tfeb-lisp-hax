;;;; Module org.tfeb.hax.trace-macroexpand of org.tfeb.hax
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.hax.trace-macroexpand"
  :description
  "A subsystem of the TFEB hax"
  :version
  "5.0.2"
  :author
  "Tim Bradshaw"
  :license
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-hax"
  :components
  ((:file "trace-macroexpand")))
