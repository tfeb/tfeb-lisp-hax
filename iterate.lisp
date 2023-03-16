;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - iterate.lisp
;; Description       - Applicative iteration
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Sat Oct  7 00:23:24 2000
;; Last Modified On  - Sat Mar 11 16:51:22 2023
;; Last Modified By  - Tim Bradshaw (tfb at pendeen.fritz.box)
;; Update Count      - 18
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; * Applicative iteration (don't need this in CMUCL)
;;;

;;; iterate.lisp is copyright 1997-2000, 2021, 2023 by me, Tim
;;; Bradshaw, and may be used for any purpose whatsoever by anyone. It
;;; has no warranty whatsoever. I would appreciate acknowledgement if
;;; you use it in anger, and I would also very much appreciate any
;;; feedback or bug fixes.
;;;
;;; The improvements to all this code in 2023, as well as the new
;;; ITERATE*, ITERATING & ITERATING* are due to Zyni: thank you.
;;;

(defpackage :org.tfeb.hax.iterate
  (:use :cl)
  (:export #:iterate #:iterate* #:iterating #:iterating*))

(in-package :org.tfeb.hax.iterate)

(provide :org.tfeb.hax.iterate)

(defun extract-ignore/other-decls (decls/forms)
  ;; See utilities.  Bit this is not the same: it returns all ignores and others as two values
  ;; others as two values.
  (do* ((ignores '())
        (others '())
        (tail decls/forms (rest tail))
        (this (first tail) (first tail)))
       ((or (null tail) (not (consp this))
            (not (eql (first this) 'declare)))
        (values (nreverse ignores) (nreverse others)))
    (if (and (consp (second this))
             (eql (first (second this)) 'ignore))
        (push this ignores)
      (push this others))))

(defun expand-iterate (name bindings body starred)
  (unless (every (lambda (binding)
                   (typecase binding
                     (symbol t)
                     (list
                      (if (<= 1 (length binding) 2)
                          t
                        (progn
                          (warn "bad binding ~S" binding))))
                     (t
                      (warn "hopeless binding ~S" binding))))
                 bindings)
    (error "bad bindings"))
  (let ((argnames
         (mapcar (lambda (binding)
                   (typecase binding
                     (symbol
                      binding)
                     (list
                      (first binding))))
                 bindings))
        (argvals
         (mapcar (lambda (binding)
                   (typecase binding
                     (symbol
                      nil)
                     (list
                      (case (length binding)
                        ((1)
                         nil)
                        ((2)
                         (second binding))))))
                 bindings)))
    (if (not starred)
        `(labels ((,name ,argnames
                    ,@body))
           (,name ,@argvals))
      (multiple-value-bind (ignores others) (extract-ignore/other-decls body)
        (declare (ignore others))
        `(labels ((,name ,argnames
                    ,@body))
           (let* ,(mapcar #'list argnames argvals)
             ,@ignores
             (,name ,@argnames)))))))

(defmacro iterate (name bindings &body body)
  "Scheme-style named-LET: parallel binding

This compiles into LABELS and recursive calls, which is fully general.
If you are using an implementation which can't optimise tail calls,
start using one which can.

This is like LET, not LET*: initial values can't see preceeding
variables."
  (expand-iterate name bindings body nil))

(defmacro iterate* (name bindings &body body)
    "Variant Scheme-style named-LET: sequential binding

This compiles into LABELS and recursive calls, which is fully general.
If you are using an implementation which can't optimise tail calls,
start using one which can.

This is like LET*: initial values can depend on preceeding variables."
  (expand-iterate name bindings body t))

(defun expand-iterating (name bindings body starred)
  (unless (every (lambda (binding)
                   (typecase binding
                     (symbol t)
                     (list
                      (if (<= 1 (length binding) 3)
                          t
                        (progn
                          (warn "bad binding ~S" binding))))
                     (t
                      (warn "hopeless binding ~S" binding))))
                 bindings)
    (error "bad bindings"))
  (let ((argnames
         (mapcar (lambda (binding)
                   (typecase binding
                     (symbol
                      binding)
                     (list
                      (first binding))))
                 bindings))
        (argvals
         (mapcar (lambda (binding)
                   (typecase binding
                     (symbol
                      nil)
                     (list
                      (case (length binding)
                        ((1)
                         nil)
                        ((2 3)
                         (second binding))))))
                 bindings))
        (argsteps
         (mapcar (lambda (binding)
                   (typecase binding
                     (symbol
                      nil)
                     (list
                      (case (length binding)
                        ((1)
                         nil)
                        ((2)
                         (second binding))
                        ((3)
                         (third binding))))))
                 bindings)))
    (if (not starred)
        (let ((secret-name (make-symbol (symbol-name name)))
              (interim-argnames (mapcar (lambda (v)
                                          (make-symbol (symbol-name v)))
                                        argnames))
              (keyword-names (mapcar (lambda (v)
                                       (intern (symbol-name v)
                                               (load-time-value (find-package "KEYWORD"))))
                                     argnames)))
          `(labels ((,secret-name ,argnames
                      (flet ((,name (&key ,@(mapcar (lambda (k v i)
                                                            `((,k ,v) ,i))
                                                    keyword-names
                                                    interim-argnames argsteps))
                               (,secret-name ,@interim-argnames)))
                        (declare (inline ,name))
                        ,@body)))
             (,secret-name ,@argvals)))
      (let ((secret-name (make-symbol (symbol-name name))))
        (multiple-value-bind (ignores others) (extract-ignore/other-decls body)
          `(labels ((,secret-name ,argnames
                      ,@ignores ,@others
                      (flet ((,name (&key ,@(mapcar #'list argnames argsteps))
                               (,secret-name ,@argnames)))
                        (declare (inline ,name))
                        ,@body)))
             (let* ,(mapcar #'list argnames argvals)
               ,@others
               (,secret-name ,@argnames))))))))

(defmacro iterating (name bindings &body body)
  "Applicative iteration macro with optional step forms: parallel binding

This is like ITERATE but each binding can be (var init/step) or (var
init step).  The local function has keyword arguments which default to
init/step or step respectively, so you can provide only some, or
simply use this as a looping construct.

This is like LET or DO, not LET* or DO*: initial values can't see
preceeding variables and step forms see the old values of variables."
  (expand-iterating name bindings body nil))

(defmacro iterating* (name bindings &body body)
  "Applicative iteration macro with optional step forms: sequentisl binding

This is like ITERATE but each binding can be (var init/step) or (var
init step).  The local function has approproate keyword arguments
which default to init/step or step respectively, so you can provide
only some, or simply use this as a looping construct.

This is like LET* or DO*, not LET or DO: initial values can see
preceeding variables and step forms can see preceeding updated
variables."
  (expand-iterating name bindings body t))
