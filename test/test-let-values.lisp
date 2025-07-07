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

(define-test ("org.tfeb.hax.let-values" "clause variants")
  (is-values (let-values (a (b 1) (c) ((d)) ((e) 2))
               (values a b c d e))
    (eql nil)
    (eql 1)
    (eql nil)
    (eql nil)
    (eql 2))
  (is-values (let-values* ((a 1) (b 2) ((c) 3) ((d e) 4 5))
               (values a b c d e))
    (eql 1)
    (eql 2)
    (eql 3)
    (eql 4)
    (eql 5)))

(define-test ("org.tfeb.hax.let-values" "starred fussy")
  (flet ((f (n)
           (ecase n
             (0 (values))
             (1 (values 1))
             (2 (values 1 2)))))
    (finish (let-values* ((a (f 1))) a))
    (finish (let-values* (((a) (f 1))) a))
    (fail (let-values* ((a (f 0))) a))
    (fail (let-values* (((a) (f 0))) a))
    (fail (let-values* (((a) (f 2))) a))
    (finish (let-values* (((a b) (f 2))) (values a b)))))

(let ((result (test "org.tfeb.hax.let-values" :report 'summary)))
  (when (eq (status result) ':failed)
    (error "Tests for let-values failed" result))
  result)
