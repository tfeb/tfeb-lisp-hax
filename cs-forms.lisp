;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - cs-forms.lisp
;; Description       - Case-sensitive forms
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Tue Jun  4 16:48:52 2002
;; Last Modified On  - Tue Jan  5 16:44:57 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 6
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Case-sensitive forms.
;;;
;;; This turns out not to be very hard.  You could use ~ on its own
;;; if you obsess about tersness.
;;;
;;; cs-forms.lisp is copyright 2002, 2021 by me, Tim Bradshaw, and may
;;; be used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.

(defpackage :org.tfeb.hax.cs-forms
  (:use :cl)
  (:export #:make-cs-form-readtable))

(in-package :org.tfeb.hax.cs-forms)

(provide :org.tfeb.hax.cs-forms)

(defun make-cs-form-readtable (&key (from nil fromp) (to nil)
                                    (toggle #\~))
  "Make a readtable suitable for reading single forms case-sensitively.

Make a copy of FROM (defaultly the current readtable) and in it make
#~ (replacing ~ by the value of TOGGLE if given) be a toggle which
causes the next form to be read case-sensitively.  While reading
case-sensitively #~ has the opposite effect: it will cause the next
form to be read case insensitively. So

  (let ((*readtable* (make-cs-form-readtable)))
    (read-from-string \"(one #~(two ThrEE #~four) five)\"))

should return (one (|two| |ThrEE| four) five) as its first value.

This all works by having a pair of readtables which are alike apart
from their case sensitivity.  Using two readtables rather than
modifying the case-sensitivity of one makes the code far more likely
to be safe, especially in the presence of multiple threads.

A consequence of the secret second readtable is that, if you want
other macro characters in the resulting readtable, you should do so
before copying it.

If #~ is adefined as a dispatch macro in the readtable being copied,
signal an error.

If TO is given, instead copy & modify FROM into TO (this behaviour is
compatible with what COPY-READTABLE does).

If TOGGLE is given, then use it as the toggle character instead of #\\~."
  (let ((cirt (if fromp (copy-readtable from to) (copy-readtable)))
        (csrt (if fromp (copy-readtable from to) (copy-readtable))))
    (when (get-dispatch-macro-character #\# toggle cirt)
      (error "Someone is already using #~A" toggle))
    (setf (readtable-case csrt) :preserve)
    (set-dispatch-macro-character
     #\# toggle
     (lambda (stream char infix)
       (declare (ignore char infix))
       (let ((*readtable* csrt))
         (read stream t nil t)))
     cirt)
    (set-dispatch-macro-character
     #\# toggle
     (lambda (stream char infix)
       (declare (ignore char infix))
       (let ((*readtable* cirt))
         (read stream t nil t)))
     csrt)
    cirt))
