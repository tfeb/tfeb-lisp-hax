;;;; Module org.tfeb.hax.binding of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.binding"
  :description
  "A subsystem of the TFEB hax"
  :version
  (:read-file-line "VERSION")
  :author
  "Tim Bradshaw"
  :license
  "MIT"
  :homepage
  "https://github.com/tfeb/tfeb-lisp-hax"
  :depends-on
  ("org.tfeb.hax.collecting" "org.tfeb.hax.iterate")
  :components
  ((:file "binding")))
