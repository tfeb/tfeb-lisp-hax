;;;; ASDF sysdcl for hax
;;;

(in-package :asdf-user)

(defsystem "org.tfeb.hax"
  :description "TFEB hax"
  :version "5.0.1"
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/tfeb-lisp-hax"
  :depends-on (#-LispWorks "closer-mop")
  :in-order-to ((test-op (load-op "org.tfeb.hax/test")))
  :components
  ((:file "collecting")
   (:file "wrapping-standard")
   (:file "iterate")
   (:file "dynamic-state")
   (:file "memoize")
   (:file "abstract-classes")
   (:file "singleton-classes")
   (:file "cs-forms")
   (:file "read-package")
   (:file "comment-form")
   (:file "define-functions")
   (:file "trace-macroexpand")
   (:file "binding"
    :depends-on ("collecting" "iterate"))
   (:file "stringtable"
    :depends-on ("collecting" "iterate"))
   (:file "object-accessors")
   (:file "utilities"
    :depends-on ("collecting" "iterate"))
   (:file "simple-loops"
    :depends-on ("collecting" "iterate" "utilities"))
   (:file "spam"
    :depends-on ("simple-loops"))
   (:file "hax-cometh"
    :depends-on ("collecting" "wrapping-standard"
                 "iterate" "dynamic-state" "memoize"
                 "abstract-classes" "singleton-classes"
                 "cs-forms" "read-package" "comment-form"
                 "define-functions" "trace-macroexpand"
                 "binding" "stringtable" "object-accessors"
                 "utilities" "simple-loops" "spam"))))

(defsystem "org.tfeb.hax/test"
  :description "TFEB hax tests"
  :version "5.0.0"
  :author "Tim Bradshaw"
  :license "MIT"
  :homepage "https://github.com/tfeb/tfeb-lisp-hax"
  :depends-on ("org.tfeb.hax" "parachute")
  :pathname "test/"
  :components
  ((:file "test-binding")
   (:file "test-iterate")
   (:file "test-collecting")))
