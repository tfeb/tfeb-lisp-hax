;;;; Tests for let-values
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.let-values
  :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.let-values/test
  (:use :cl :org.tfeb.hax.let-values :org.shirakumo.parachute))

(in-package :org.tfeb.hax.let-values/test)

(define-test "org.tfeb.hax.let-values")

(defun value-of-/a/ ()
  (declare (special /a/))
  /a/)

(define-test ("org.tfeb.hax.let-values" "special bindings")
  (let ((c (cons 1 1))
        (/a/ (cons 2 2)))
    (declare (special /a/))
    (false (eq /a/ c))
    (is-values (let-values (((/a/) c)
                            ((b) (value-of-/a/)))
                 (declare (special /a/))
                 (values /a/ b))
      (eq c)
      (eq /a/))
    (is-values (let*-values (((/a/) c)
                             ((b) (value-of-/a/)))
                 (declare (special /a/))
                 (values /a/ b))
      (eq c)
      (eq c))))

(test "org.tfeb.hax.let-values" :report 'summary)
