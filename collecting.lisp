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
          #:with-collectors
          #:with-accumulators))

(in-package :org.tfeb.hax.collecting)

(provide :org.tfeb.hax.collecting)

(defmacro collecting (&body forms)
  "Collect things into a list forwards.

Within the body of this macro The form `(COLLECT THING)' will collect
THING into the list returned by COLLECTING.  COLLECT is a local
function so can be passed as an argument, or returned.  COLLECT
returns its argument. See WITH-COLLECTORS for which this COLLECTING is
now a shim"
  `(with-collectors (collect) ,@forms))
                          
(defmacro with-collectors ((&rest collectors) &body forms)
  ;; multiple-collector version of COLLECTING.
  "Collect some things into lists forwards.  

The names in COLLECTORS are defined as local functions, which each
collect into a separate list.  The collector functions return their
argument.  Returns as many values as there are collectors.

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

(defmacro with-accumulators ((&rest accumulators) &body forms)
  ;; A general version of WITH-COLLECTORS.  I don't think a dedcated
  ;; single-accumulator version of this makes any kind of useful
  ;; sense.
  "Accumulate some things

This defines some local functions which accumulate things as described
by ACCUMULATORS.  Each accumulator is given as either a simple
specifation or a more extensible one which allows more options.

A simple specification is (name operator &optional initially), where
name is the name of the local accumulator function, operator names the
operator and initially, if given is the initial value.

operator denotes (it's a symbol or a lambda expression) a function
wwhich can take zero or two arguments.  If there is no initial value
it is called with no arguments to initialise the accumulator.
Otherwise it will be called, each time the accumulator function is
called, with two arguments: the current value of the accumulator and
the argument to the accumulator function.  Its return value is the new
value of the accumulator.

The extensible specification is (name operator &key initially type
returner).  In this case name, operator & initially mean exactly the
same as previously, but type is a type specification for the variable
which underlies the accumulator, and returner denotes a function of
one argument, the final value of the accumulator, whose return value
is used instead of the final value.  There may in future be additional
keywords.

The local accumulator functions are declared inline, and return their
argument.

The form returns the final values of all the accumulators as multiple
values, possibly via the returner functions."
  (flet ((parse-accumulator (a)
           ;; Turn an accumulator into a plist
           (typecase a
             (cons
              (case (length a)
                ((1)
                 (error "accumulaor ~S has no operator" a))
                ((2 3)
                 (destructuring-bind (name on &optional (init `(,on))) a
                   (unless (symbolp name)
                     (error "the name of accumulator ~S isn't a symbol" a))
                   (unless (or (symbolp on)
                               (and (consp on)
                                    (eql (first on) 'lambda)))
                     (error "the operator of accumulator ~S isn't a symbol or lambda"
                            a))
                   `(name ,name on ,on init ,init)))
                (otherwise
                 (destructuring-bind (name on &key
                                           (initially `(,on))
                                           (type nil)
                                           (returner nil)) a
                   (unless (symbolp name)
                     (error "the name of accumulator ~S isn't a symbol" a))
                   (unless (or (symbolp on)
                               (and (consp on)
                                    (eql (first on) 'lambda)))
                     (error "the operator of accumulator ~S~
isn't a symbol or lambda expression" a))
                   (unless (or (symbolp returner)
                               (and (consp returner)
                                    (eql (first returner) 'lambda)))
                     (error "the return operator of accumulator ~S~
isn't a symbol of lambda expression" a))
                   `(name ,name on ,on init ,initially
                          type ,type returner ,returner)))))
             (t
              (error "hopeless accumulator ~S" a))))
         (getter (property &optional (default nil))
           (lambda (plist)
             (getf plist property default))))
    (let* ((parsed (mapcar #'parse-accumulator accumulators))
           (names (mapcar (getter 'name) parsed))
           (inits (mapcar (getter 'init) parsed))
           (types (mapcar (getter 'type) parsed))
           (returners (mapcar (getter 'returner) parsed))
           (ons (mapcar (getter 'on) parsed))
           (vns (mapcar (lambda (name) (make-symbol (symbol-name name)))
                        names)))
      `(let ,(mapcar #'list vns inits)
         ,@(mapcan (lambda (v tp)
                     (if tp `((declare (type ,tp ,v))) '()))
                   vns types)
         (flet ,(mapcar (lambda (name on vn)
                          `(,name (it) (setf ,vn (,on ,vn it)) it))
                        names ons vns)
           (declare (inline ,@names))
           ,@forms)
         (values ,@(mapcar (lambda (vn returner)
                             (if returner
                                 `(,returner ,vn)
                               vn))
                           vns returners))))))
