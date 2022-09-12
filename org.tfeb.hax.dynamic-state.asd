;;;; Module org.tfeb.hax.dynamic-state of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.dynamic-state"
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
  :components
  ((:file "dynamic-state")))
