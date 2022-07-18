;;;; Module org.tfeb.hax.read-package of org.tfeb.hax
;;;

(in-package :asdf-user)

(asdf/parse-defsystem:defsystem "org.tfeb.hax.read-package"
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
  ((:file "read-package")))
