;;;; Module org.tfeb.hax.object-accessors of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.object-accessors"
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
  "utilities"
  :components
  ((:file "object-accessors")))
