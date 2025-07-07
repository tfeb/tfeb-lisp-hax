;;;; Simple loop tests
;;;
;;; Mostly these are to check the values stuff.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.simple-loops :compile t)
  #+Quicklisp
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.simple-loops/test
  (:use :cl :org.tfeb.hax.simple-loops :org.shirakumo.parachute))

(in-package :org.tfeb.hax.simple-loops/test)

(define-test "org.tfeb.hax.simple-loops")

(define-test ("org.tfeb.hax.simple-loops" "doing values")
  (is-values (doing ((i 0 (1+ i)))
                 ((> i 1)))
    (= 2))
  (is-values (doing ((i 0 (1+ i))
                     (j 0))
                 ((> i 1)))
    (= 2)
    (= 0))
  (is-values (doing ((i 0 (1+ i)))
                 ((> i 1) 1))
    (= 1))
  (is-values (doing ((i 0 (1+ i)))
                 ((> i 1) 1 2))
    (= 1)
    (= 2))
  (is-values (doing ((i 0 (1+ i)))
                 ((> i 1) (values 1 2)))
    (= 1)
    (= 2))
  (is-values (doing ((i 0 (1+ i)))
                 ((> i 1) (values 1 2) 3))
    (= 1)
    (= 2)
    (= 3)))

(define-test ("org.tfeb.hax.simple-loops" "looping/values inits")
  (is-values (looping/values ((i j) (values 0 1))
               (return (values i j)))
    (= 0)
    (= 1))
  (is-values (looping/values ((i j) 0 1)
               (return (values i j)))
    (= 0)
    (= 1))
  (is-values (looping/values ((i j k) (values 0 1) 2)
               (return (values i j k)))
    (= 0)
    (= 1)
    (= 2)))

(define-test ("org.tfeb.hax.simple-loops" "looping/values* iterate")
  (is = (looping/values* (((i j) (values 0 1))
                          ((k) 0))
          (when (> k 3)
            (return k))
          (values i j (1+ k)))
      4))

(define-test ("org.tfeb.hax.simple-loops" "looping/values* sequential")
  (finish (looping/values* (((i j) 0 1)
                            ((k) i))
            (return t))))

(let ((result (test "org.tfeb.hax.simple-loops" :report 'summary)))
  (when (eq (status result) ':failed)
    (error "Tests for simple loops failed" result))
  result)
