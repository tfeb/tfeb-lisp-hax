;;;; Module org.tfeb.hax.let-values of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.let-values"
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
  ("org.tfeb.hax.spam"
   "org.tfeb.hax.collecting"
   "org.tfeb.hax.iterate"
   "org.tfeb.hax.utilities"
   "org.tfeb.hax.process-declarations")
  :components
  ((:file "let-values")))
