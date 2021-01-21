;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - dynamic-state.lisp
;; Description       - Dynamic state access
;; Author            - Tim Bradshaw (tfb at KINGSTON)
;; Created On        - Wed Feb 21 09:10:49 2001
;; Last Modified On  - Mon Jan 18 10:51:01 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 24
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Dynamic state access
;;;
;;; dynamic-state.lisp is copyright 2001, 2012 by me, Tim Bradshaw, and
;;; may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.

(defpackage :org.tfeb.hax.dynamic-state
  (:use :cl)
  (:export #:define-dynamic-state))

(in-package :org.tfeb.hax.dynamic-state)

(provide :org.tfeb.hax.dynamic-state)

(defmacro define-dynamic-state ((binder accessor) &body all-specials)
  ;; define a binder, BINDER, and accessor, ACCESSOR for some dynamic
  ;; state variables.  The legal variables must come from
  ;; ALL-SPECIALS.
  `(progn
     (defmacro ,binder (bindings &body body)
       ;; Establish a dynamic state: binding specs like LET.
       (let ((varnames
              (mapcan #'(lambda (b)
                          (typecase b
                            (symbol
                             (unless (member b ',all-specials)
                               (error "~S is not a valid dynamic state variable for ~S"
                                      b ',binder))
                             (list b))
                            (cons
                             (unless (and (= (length b) 2)
                                          (symbolp (first b)))
                               (error "~S is not a valid binding specification" b))
                             (unless (member (first b) ',all-specials)
                               (error "~S is not a valid dynamic state variable for ~S"
                                      (first b) ',binder))
                             (list (first b)))
                            (t
                             (error "~S is not a valid binding specification" b))))
                      bindings)))
         ;; try and generate slightly reasonable-looking code.
         (if (not (null varnames))
             `(let ,bindings
                (declare (special ,@varnames))
                ,@body)
             `(locally
                ,@body))))

     (defmacro ,accessor (varnames &body body)
       ;; get access to a dynamic state -- VARNAMES is list of
       ;; variables we want to see.
       (dolist (v varnames)
         (unless (symbolp v)
           (error "~S is not a valid binding specification" v))
         (unless (member v ',all-specials)
           (error "~S is not a valid dynamic state variable for ~S"
                  v ',accessor)))
       ;; try and generate slightly reasonable-looking code.
       (if (not (null varnames))
           `(locally
                (declare (special ,@varnames))
              ,@body)
           `(locally
              ,@body)))
     '(,binder ,accessor)))

#||
(define-dynamic-state (with-dynamic-state with-dynamic-state-access)
   error-code result)

(defun foo (x)
  (with-dynamic-state ((result x))
    (bar)
    (values (let ((result 10))
              ;; This RESULT is *lexical*, so this closes over it
              #'(lambda (x)
                  (cons x result)))
            result)))

(defun bar ()
  (let ((result 12))
    ;; This closure closes over the *lexical* RESULT we have here.
    (henry #'(lambda (x)
               (cons x result)))))

(defun henry (fn)
  (with-dynamic-state-access (result)
    (setf result fn)))
||#
