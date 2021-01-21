;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - iterate.lisp
;; Description       - Applicative iteration
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Sat Oct  7 00:23:24 2000
;; Last Modified On  - Mon Jan 18 15:50:00 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 14
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; * Applicative iteration (don't need this in CMUCL)
;;;
;;; Note all these forms do sequential bindings, like LET*.
;;;

;;; iterate.lisp is copyright 1997-2000 by me, Tim Bradshaw, and may
;;; be used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.

(defpackage :org.tfeb.hax.iterate
  (:use :cl)
  (:export #:iterate))

(in-package :org.tfeb.hax.iterate)

(provide :org.tfeb.hax.iterate)

(defconstant +tr-implementation-p+ t
  ;; (previously this was NIL for Genera only)
  "Can this implementation perform tail-call optimisation?
Specifically can it optimise (LABELS ((X (...) ... (X ...) ...))),
where the call to X is in tail position, given reasonable constraints
(no special bindings, in the case of CMU for instance")

(defmacro iterate (name bindings &body body)
  "Scheme-style named-LET, with hacks.

For implementations which can do TRO, this compiles into LABELS
and recursive calls, which is fully general.

For implementations which can not, *if* the name contains the word `LOOP'
(any case), then this compiles into something dreadful using BLOCK and
RETURN-FROM, which is fast for looping but can't be used recursively.
If the name does *not* contain `LOOP' it compiles into LABELS as above.

Notes: bindings are sequential not parallel (because LABELS is), so this is
like LET* not LET.  The local function defined should be considered to have
dynamic extent."
 (cond  (+tr-implementation-p+
         ;; labels is free
         `(iterate/labels ,name ,bindings ,@body))
        ((search "LOOP" (string-upcase (symbol-name name)))
         ;; not TR, and we asked for a loop
         `(iterate/tag ,name ,bindings ,@body))
        (t
         ;; not TR, not loop
         `(iterate/labels ,name ,bindings ,@body))))

(defmacro iterate/labels (name bindings &body body)
  ;; this one is the fully-fledged variant:  note that this is like LET*
  ;; not LET
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
                 #+Genera(declare (sys:downward-function))
                 ,@body))
        (,name ,@argvals)))))

(defmacro iterate/tag (tag bindings &body body)
  ;; this is the hacky one!  It really, really is a hack, believe me.
  ;; Note that the bindings are in sequence, not pll -- because ITERATE
  ;; is (accidentally), and I want this to be the same.  I presume that
  ;; compilers get code that is as good for PROG* as PROG if there are
  ;; no dependencies.
  (let ((argnames (mapcar #'(lambda (binding)
                              (etypecase binding
                                (symbol binding)
                                (cons (car binding))))
                          bindings)))
    ;; this used to use PROG*, but it's not clear if that really
    ;; portably allows an initial symbol as a name for the block.
    ;; This does (and gets the same code).
    `(block ,tag
       (let* ,bindings
         (tagbody
           ,tag
            (macrolet ((,tag (&rest args)
                        `(progn
                           (setf ,@(mapcan #'(lambda (name val)
                                             (list name val))
                                         ',argnames args))
                           (go ,',tag))))
              ;; it's a pain that all the GO-containing forms need to be
              ;; RETURNed from explicitly rather than returning the last
              ;; value.  On the SB this means the body of the loop is
              ;; one instruction bigger than DOTIMES, because the RETURN
              ;; compiles into a conditional branch (see below for
              ;; samples).  Anyway that's why this slightly obscure
              ;; thing is done.
              (return-from ,tag (progn ,@body))))))))

#||
;;; (ITERATE/TAG was formerly called TAGLET)
;;; trivial tests of TAGLET.
;;;
;;; I wondered why it was a bit slower than DOTIMES...
;;;
(defun ts (n)
  ;; trying to be a loop
  (taglet loop ((i 0))
    (if (< i n)
        (loop (1+ i))
        i)))

;;; TS compiles on a 36xx as
;;;
;  0  ENTRY: 1 REQUIRED, 0 OPTIONAL
;  1  PUSH-IMMED 0               ;creating I(FP|1)
;  2  PUSH-LOCAL FP|1            ;I
;  3  PUSH-LOCAL FP|0            ;N
;  4  BUILTIN INTERNAL-< STACK
;  5  BRANCH-FALSE 10
;  6  BUILTIN +-INTERNAL STACK 1
;  7  BRANCH 2
; 10  RETURN-STACK
;;;
;;; note the branch at 5 which somes from the RETURN

(defun tsloop (n)
  ;; actually a loop
  (dotimes (i n i)))

;;; TSLOOP compiles on a 36xx as
;;;
;  0  ENTRY: 1 REQUIRED, 0 OPTIONAL
;  1  PUSH-IMMED 0               ;creating I(FP|1)
;  2  PUSH-LOCAL FP|0            ;N
;  3  BUILTIN PLUSP STACK
;  4  BRANCH-FALSE 12
;  5  BUILTIN +-INTERNAL STACK 1
;  6  PUSH-LOCAL FP|1            ;I
;  7  PUSH-LOCAL FP|0            ;N
; 10  BUILTIN INTERNAL-< STACK
; 11  BRANCH-TRUE 5
; 12  RETURN-STACK
;;;
;;; there is no branch in the loop here so this is a bit faster.  Of
;;; course a decent compiler would probably deal with this anyway.
||#
