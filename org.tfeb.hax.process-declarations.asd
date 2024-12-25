;;;; Module org.tfeb.hax.process-declarations of org.tfeb.hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax.process-declarations"
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
  ("org.tfeb.hax.utilities")
  :components
  ((:file "process-declarations")))
