;;;; Some tests for org.tfeb.hax.binding
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.binding
 #+Quicklisp
 ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.binding/test
  (:use :cl :org.tfeb.hax.binding :org.shirakumo.parachute))

(in-package :org.tfeb.hax.binding/test)

(define-test "org.tfeb.hax.binding")

(define-test ("org.tfeb.hax.binding" "macroexpansion")
  (macrolet ((tests (&body forms/expansions)
               `(progn
                  ,@(mapcar (lambda (form/expansion)
                              (destructuring-bind (form expansion) form/expansion
                                `(is equal (macroexpand-1 ',form) ',expansion)))
                            forms/expansions))))
    (tests
     ((binding
        (bind a 1)
        (bind b 2)
        (values a b))
      (let* ((a 1) (b 2))
        (values a b)))
     ((binding
        1
        (bind b 2)
        b)
      (progn
        1
        (let* ((b 2))
          b)))
     ((binding
        (bind (f &rest args) args)
        (f 1 3))
      (labels ((f (&rest args)
                 (binding args)))
        (f 1 3)))
     ((binding
        (bind (f x)
          (declare (type fixnum x))
          x)
        (f 1))
      (labels ((f (x)
                 (declare (type fixnum x))
                 (binding x)))
        (f 1)))
     ((binding
        (bind/values (a b) (values 1 2))
        (values a b))
      (multiple-value-bind (a b) (values 1 2)
        (declare (ignore))
        (values a b)))
     ((binding
        (bind/values (a b) 1 2)
        (values a b))
      (multiple-value-bind (a b) (values 1 2)
        (declare (ignore))
        (values a b)))
     ((binding
        (bind/destructuring (a &rest b) (list 1 2))
        (values a b))
      (destructuring-bind (a &rest b) (list 1 2)
        (values a b)))
     ((binding
        (bind/macro (x y) y)
        z)
      (macrolet ((x (y) (binding y)))
        z))
     ((binding
        (bind/macro a 2)
        a)
      (symbol-macrolet ((a 2))
        a)))))

(let ((result (test "org.tfeb.hax.binding" :report 'summary)))
  (when (eq (status result) ':failed)
    (error "Tests for binding failed"))
  result)
