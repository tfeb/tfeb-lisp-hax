;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - read-package.lisp
;; Description       - Read-time package hacks
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Fri Jul  6 12:13:59 2001
;; Last Modified On  - Tue Jan  5 16:44:47 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 16
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Symbolics-style read-time packages.
;;;
;;; read-package.lisp is copyright 2001, 2021 by me, Tim Bradshaw, and
;;; may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.

;;; In genera you could give package prefixes to entire forms:
;;;     cl-user:(cons 1 2)
;;; meant `read in package CL-USER.
;;;
;;; This can't easily be implemented in standard CL, but something
;;; similar can.  This file implements a dispatching macro char, @,
;;; which forces the following form to be read in the package named:
;;; #@cl-user (cons 1 2) does about the same thing as above.
;;;
;;; The principle hack here is that CL gives no hook into the reader
;;; before it interns things, so you can't say `give me a token', but
;;; have to accept something like a symbol.  Hence the hack of using a
;;; secret package in which things get interned & then uninterned (to
;;; avoid leaks).
;;;
;;; This is not likely to be particularly safe code
;;;

(defpackage :org.tfeb.hax.read-package
  (:use :cl)
  (:export #:make-read-package-readtable))

(in-package :org.tfeb.hax.read-package)

(provide :org.tfeb.hax.read-package)

(defvar *read-package-package*
  (make-package "READ-PACKAGE-PACKAGE" :use '()))

(defun make-read-package-readtable (&key (from nil fromp) (to nil)
                                         (at #\@))
  "Make readtable with read-time package support.

This is a readtable which is a copy of FROM (defaultly the current
readtable), but which has #@ (or as specified below) defined such that
#@pkg will read the next form in the package denoted by pkg.  So

(let ((*readtable* (make-cs-form-readtale)))
  (read-from-string \"(foo #@keyword bar)\"))

should return (foo :bar) as its first value.

If #@ is adefined as a dispatch macro in the readtable being copied,
raise an error.

If TO is given, instead copy & modify FROM into TO (this behaviour is
compatible with what COPY-READTABLE does).

If AT is given, it is the dispatch macro character to use instead of #\@."

  (let ((rt (if fromp (copy-readtable from to) (copy-readtable))))
    (when (get-dispatch-macro-character #\# at rt)
      (error "Someone is already using #~A" at))
    (set-dispatch-macro-character
     #\# at
     #'(lambda (stream char infix)
         (declare (ignore char infix))
         (let* ((*package* *read-package-package*)
                (tok (read stream t nil t))
                (string (typecase tok
                          (symbol
                           (if (eq (symbol-package tok) *read-package-package*)
                               (unintern tok)
                             (warn
                              "Dubious syntax for read-package: symbol in package ~A"
                              (package-name (symbol-package tok))))
                           (symbol-name tok))
                          (string
                           (warn "Dubious syntax for read-package: string read")
                           tok)
                          (t
                           (error "read-package: got a ~A, expecting a symbol"
                                  (type-of tok)))))
                (package (find-package string)))
           (unless package
             (error "No package with name ~A for read-package" string))
           (let ((*package* package))
             (read stream t nil t))))
     rt)
    rt))
