;;;; Module org.tfeb.hax.define-functions of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.define-functions"
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
  ((:file "define-functions")))
