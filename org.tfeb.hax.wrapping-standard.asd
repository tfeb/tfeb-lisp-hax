;;;; Module org.tfeb.hax.wrapping-standard of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.wrapping-standard"
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
  ((:file "wrapping-standard")))
