;;;; Tests for iterate
;;; to check I am actually getting it right
;;;


#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.iterate
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.iterate/test
  (:use :cl :org.tfeb.hax.iterate :org.shirakumo.parachute))

(in-package :org.tfeb.hax.iterate/test)

(define-test "org.tfeb.hax.iterate")

(define-test ("org.tfeb.hax.iterate" "binding stupidity")
  ;; Just confirm that ITERATE is like LET because I was confused
  ;; about this for ever.
  (let ((i 1) (j 2))
    (is-values (iterate n ((i j) (k i))
                 (values i k))
      (eql 2)
      (eql 1)))
  (let ((x 3))
    (is eql (iterate n ((x 1) (y x))
              (declare (ignorable x))
              y)
        3)))

(test "org.tfeb.hax.iterate" :report 'summary)
