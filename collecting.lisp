;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - collecting.lisp
;; Description       - Collecting lists forwards
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - 1989
;; Last Modified On  - Mon Jan 25 20:14:09 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 17
;; Status            - Unknown
;; 
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Collecting lists forwards
;;; This is an old macro cleaned up a bit
;;;
;;; 2012: I have changed this to use local functions rather than macros,
;;; on the assumption that implementations can optimize this pretty well now
;;; and local functions are much semantically nicer than macros.
;;;
;;; 2021: I have, finally, defined COLLECTING in terms of
;;; WITH-COLLECTORS: I was only holding onto the old macro out of
;;; nostalgia, and that's a bad reason.
;;;

;;; These macros hardly seem worth copyrighting, but are copyright
;;; 1989-2012, 2021 by me, Tim Bradshaw, and may be used for any
;;; purpose whatsoever by anyone. There is no warranty whatsoever. I
;;; would appreciate acknowledgement if you use this in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;;

(defpackage :org.tfeb.hax.collecting
  (:use :cl)
  (:export #:collecting
          #:collect
          #:with-collectors))

(in-package :org.tfeb.hax.collecting)

(provide :org.tfeb.hax.collecting)

(defmacro collecting (&body forms)
  "Collect things into a list forwards.

Within the body of this macro The form `(COLLECT THING)' will collect
THING into the list returned by COLLECTING.  COLLECT is a local
function so can be passed as an argument, or returned. See
WITH-COLLECTORS for which this COLLECTING is now a shim"
  `(with-collectors (collect) ,@forms))
                          
(defmacro with-collectors ((&rest collectors) &body forms)
  ;; multiple-collector version of COLLECTING.
  "Collect some things into lists forwards.  

The names in COLLECTORS are defined as local functions, which each
collect into a separate list.  Returns as many values as there are
collectors.

The local functions defined by this macro are declared inline and so
should have no overhead for a reasonable compiler.  The macro uses
secret tail pointers and so should be efficient."
  (let ((cvns (mapcar #'(lambda (c)
                          (make-symbol (concatenate 'string
                                                    (symbol-name c) "-VAR")))
                      collectors))
        (ctns (mapcar #'(lambda (c)
                          (make-symbol (concatenate 'string
                                                    (symbol-name c) "-TAIL")))
                      collectors)))
    `(let (,@cvns ,@ctns)
       (flet ,(mapcar (lambda (cn cvn ctn)
                        `(,cn (it)
                              (if ,cvn
                                  (setf (cdr ,ctn) (list it)
                                        ,ctn (cdr ,ctn))
                                (setf ,ctn (list it)
                                      ,cvn ,ctn))
                              it))
                      collectors cvns ctns)
         (declare (inline ,@collectors))
         ,@forms)
       (values ,@cvns))))
