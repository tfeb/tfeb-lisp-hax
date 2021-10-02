;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - iterate.lisp
;; Description       - Applicative iteration
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Sat Oct  7 00:23:24 2000
;; Last Modified On  - Sat Oct  2 13:11:38 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 16
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; * Applicative iteration (don't need this in CMUCL)
;;;

;;; iterate.lisp is copyright 1997-2000, 2021 by me, Tim Bradshaw, and
;;; may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.

(defpackage :org.tfeb.hax.iterate
  (:use :cl)
  (:export #:iterate))

(in-package :org.tfeb.hax.iterate)

(provide :org.tfeb.hax.iterate)

(defmacro iterate (name bindings &body body)
  "Scheme-style named-LET

This compiles into LABELS and recursive calls, which is fully general.
If you are using an implementation which can't optimise tail calls,
start using one which can.

This is like LET, not LET*. The local function defined should be
considered to have dynamic extent."
  (let ((argnames ())
        (argvals ()))
    (labels ((grind-bindings (tail)
               (if (not (null tail))
                   (etypecase (car tail)
                     (symbol
                      (grind-bindings (cdr tail))
                      (push (car tail) argnames)
                      (push nil argvals))
                     (list
                      (grind-bindings (cdr tail))
                      (push (car (car tail)) argnames)
                      (push (cadr (car tail)) argvals))))))
      (grind-bindings bindings)
      `(labels ((,name ,argnames
                  ,@body))
        (,name ,@argvals)))))
