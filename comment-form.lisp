;;;; Commenting forms
;;;
;;; Racket has #; to comment a form.  You can do this in CL with
;;; #+(or), but I thought an explicit equivalent might be interesting.
;;; This comes from an answer on stack overflow:
;;; https://stackoverflow.com/a/65649186/5920214
;;;
;;; comment-form.lisp is copyright 2021 by me, Tim Bradshaw, and may
;;; be used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.

(defpackage :org.tfeb.hax.comment-form
  (:use :cl)
  (:export #:make-comment-form-readtable))

(in-package :org.tfeb.hax.comment-form)

(provide :org.tfeb.hax.comment-form)

(defun make-comment-form-readtable (&key (from *readtable*) (to nil)
                                         (semicolon #\;))
  "Make a readtable with comment-form readmacro (#; by default).

Make a copy of FROM (defaultly the current readtable) and in it make
#; (or as below) be a read macro which causes the next form to be
skipped.  #n; will skip n forms. Other than the skipping-n-forms
thing, this is no more than what #+(or) will do, but it is clearer I
think.

If TO is given, instead copy & modify FROM into TO (this behaviour is
compatible with what COPY-READTABLE does).

If SEMICOLON is given, it is the dispatching character, instead of #\\;."
  (let ((cfrt (copy-readtable from to)))
    (when (get-dispatch-macro-character #\# semicolon cfrt)
      (error "Someone is already using #~A" semicolon))
    (set-dispatch-macro-character
     #\# semicolon
     (lambda (stream char n)
       (declare (ignore char))
       (let ((*read-suppress* t))
         (dotimes (i (or n 1) (values))
           (read stream))))
     cfrt)
    cfrt))
