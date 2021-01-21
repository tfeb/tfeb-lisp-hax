;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - collecting.lisp
;; Description       - Collecting lists forwards
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - 1989
;; Last Modified On  - Sun Aug 30 14:06:06 2020
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 15
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

;;; These macros hardly seem worth copyrighting, but are copyright
;;; 1989-2012 by me, Tim Bradshaw, and may be used for any purpose
;;; whatsoever by anyone. There is no warranty whatsoever. I would
;;; appreciate acknowledgement if you use this in anger, and I would
;;; also very much appreciate any feedback or bug fixes.

(defpackage :org.tfeb.hax.collecting
  (:use :cl)
  (:export #:collecting
          #:collect
          #:with-collectors))

(in-package :org.tfeb.hax.collecting)

(provide :org.tfeb.hax.collecting)

(defmacro collecting (&body forms)
  ;; Collect some random stuff into a list by keeping a tail-pointer
  ;; to it, return the collected list.  This now uses a local function
  ;; rather than a macro.
  "Collect things into a list forwards.  Within the body of this macro
   The form `(COLLECT THING)' will collect THING into the list returned by 
   COLLECTING.  COLLECT is a local function so can be passed as an argument,
   or returned. Uses a tail pointer -> efficient."
  (let ((cn (make-symbol "C")) (tn (make-symbol "CT")))
    `(let ((,cn '()) (,tn nil))
       (flet ((collect (it)
                (if ,cn
                    (setf (cdr ,tn) (list it)
                          ,tn (cdr ,tn))
                  (setf ,tn (list it)
                        ,cn ,tn))
                it))
         (declare (inline collect))
         ,@forms)
       ,cn)))
                          
(defmacro with-collectors ((&rest collectors) &body forms)
  ;; multiple-collector version of COLLECTING.
  "Collect some things into lists forwards.  
The names in COLLECTORS are defined as local functions, which each collect into a 
separate list.  Returns as many values as there are collectors."
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
