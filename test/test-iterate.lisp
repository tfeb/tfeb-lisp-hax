;;;; Tests for iterate
;;; to check I am actually getting it right
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.iterate :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.iterate/test
  (:use :cl :org.tfeb.hax.iterate :org.shirakumo.parachute))

(in-package :org.tfeb.hax.iterate/test)

(define-test "org.tfeb.hax.iterate")

(define-test ("org.tfeb.hax.iterate" "iterate like LET")
  ;; Just confirm that ITERATE is like LET because I was confused
  ;; about this for ever.
  (let ((i 1) (j 2))
    (is-values (iterate n ((i j) (k i))
                 (values i k))
      (= 2)
      (= 1)))
  (let ((x 3))
    (is eql 3 (iterate n ((x 1) (y x))
              (declare (ignorable x))
              y))))

(define-test ("org.tfeb.hax.iterate" "iterate* like LET*")
  (let ((i 1) (j 2))
    (declare (ignorable i))
    (is-values (iterate* n ((i j) (k i))
                 (values i k))
      (= 2)
      (= 2)))
  (let ((x 3))
    (declare (ignorable x))
    (is eql 1 (iterate* n ((x 1) (y x))
                (declare (ignorable x))
                y))))

;;; Test that the scope of local functions is correct: they were wrong for a
;;; very long time
;;;

(define-test ("org.tfeb.hax.iterate" "iterate local function not in scope of initforms")
  (is eql 1
      (let ((v 0))
        (flet ((foo (x)
                 (setf v x)))
          (iterate foo ((x (foo 1)))
            (declare (ignore x))
            v)))))

(define-test ("org.tfeb.hax.iterate" "iterate* local function not in scope of initforms")
  (is-values
      (let ((v 0))
        (flet ((foo (x)
                 (setf v x)))
          (iterate* foo ((x (foo 1)) (y (1+ x)))
            (declare (ignore x))
            (values v y))))
    (= 1)
    (= 2)))

(define-test ("org.tfeb.hax.iterate" "iterating basic")
  (is = 11 (iterating n ((i 0 (1+ i)))
             (if (> i 10) i (n))))
  (is-values (iterating n ((i 0 (1+ i))
                           (j 0))
               (if (> i 10) (values i j) (n)))
    (= 11)
    (= 0)))

(define-test ("org.tfeb.hax.iterate" "iterating like LET")
  (is-values (iterating n ((i 0 (1+ i))
                           (j 0 i))
               (if (> i 10) (values i j) (n)))
    (= 11)
    (= 10))
  (is-values (iterating n ((i 0) (j 0 i))
               (if (> i 10) (values i j) (n :i (1+ i))))
    (= 11)
    (= 10))
  (is-values (let ((x 1))
               (iterating n ((x 2) (y x))
                   (values x y)))
    (= 2)
    (= 1)))

(define-test ("org.tfeb.hax.iterate" "iterating* like LET*")
  (is-values (iterating* n ((i 0 (1+ i))
                           (j 0 i))
               (if (> i 10) (values i j) (n)))
    (= 11)
    (= 11))
  (is-values (iterating* n ((i 0) (j 0 i))
               (if (> i 10) (values i j) (n :i (1+ i))))
    (= 11)
    (= 11))
  (is-values (let ((x 2))
               (declare (ignorable x))
               (iterating* n ((x 1) (y x))
                 (values x y)))
    (= 1)
    (= 1)))

(define-test ("org.tfeb.hax.iterate" "iterating keyword arguments")
  (is-values (iterating n ((i 0 (1+ i))
                           (j 1))
               (if (< i 10)
                   (n :j (1- i))
                 (values i j)))
    (= 10)
    (= 8)))

(test "org.tfeb.hax.iterate" :report 'summary)
