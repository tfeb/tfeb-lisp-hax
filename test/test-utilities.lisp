;;;; Some rudimentary tests for some of the utilities
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.utilities
 #+Quicklisp
 ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.utilities/test
  (:use :cl :org.tfeb.hax.utilities :org.shirakumo.parachute))

(in-package :org.tfeb.hax.utilities/test)

(define-test "org.tfeb.hax.utilities")

(define-test ("org.tfeb.hax.utilities" "parse-docstring-body")
  (is-values (parse-docstring-body '())
    (equal nil)
    (equal '())
    (equal '()))
  (is-values (parse-docstring-body '("foo"))
    (equal nil)
    (equal '())
    (equal '("foo")))
  (is-values (parse-docstring-body '("foo" (declare) 1))
    (equal "foo")
    (equal '((declare)))
    (equal '(1)))
  (is-values (parse-docstring-body
              '((declare 1)
                "foo"
                (declare 2)
                "foo"))
    (equal "foo")
    (equal '((declare 1) (declare 2)))
    (equal '("foo")))
  (is-values (parse-docstring-body
              '((declare 1)
                "foo"
                (declare 2)))
    (equal "foo")
    (equal '((declare 1) (declare 2)))
    (equal '())))

(test "org.tfeb.hax.utilities" :report 'summary)
