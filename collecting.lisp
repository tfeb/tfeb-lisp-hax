;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - collecting.lisp
;; Description       - Collecting lists forwards
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - 1989
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
;;; 1989-2012, 2021-2022 by me, Tim Bradshaw, and may be used for any
;;; purpose whatsoever by anyone. There is no warranty whatsoever. I
;;; would appreciate acknowledgement if you use this in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;;

(defpackage :org.tfeb.hax.collecting
  (:use :cl)
  (:export #:collecting
          #:collect
          #:with-collectors
          #:collecting-values
          #:with-accumulators
          #:make-collector
          #:collector-contents
          #:collect-into
          #:nconc-collectors
          #:nconc-collector-onto))

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

(defmacro collecting-values ((&rest collectors) &body form/s)
  ;; Based on an idea by Zyni
  "Collect multiple values withing COLLECTING

COLLECTORS should be the names of bound collectors.  If there is a
single other argument this is assumed to be a form which will return
multiple values.  There should either be a single form in the body
which is assumed to return multiple values, or exacly as many forms as
there are collectors with one value being collected from each.

Return the values collected."
  (let ((cl (length collectors))
        (fl (length form/s)))
    (unless (or (= fl 1)
                (= fl cl))
      (error "need either a single form or as many forms as collectors"))
    (if (= cl 1)
        `(,(first collectors)  ,(first form/s))
      `(multiple-value-bind ,collectors ,(if (= fl 1)
                                             (first form/s)
                                           `(values ,@form/s))
         (values ,@(mapcar #'list collectors collectors))))))

(defmacro with-accumulators ((&rest accumulators) &body forms)
  ;; A general version of WITH-COLLECTORS.  I don't think a dedcated
  ;; single-accumulator version of this makes any kind of useful
  ;; sense.
  "Accumulate some things

This defines some local functions which accumulate things as described
by ACCUMULATORS.  Each accumulator is given as either a simple
specification or a more extensible one which allows more options.

A simple specification is (name operator &optional initially), where
name is the name of the local accumulator function, operator names the
operator and initially, if given is the initial value.

operator denotes (it's a symbol or a lambda expression) a function
which can take zero or two arguments.  If there is no initial value
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

;;;; Something more like Interlisp-D's DOCOLLECT / ENDCOLLECT / TCONC
;;; See interlisp.org/docs/IRM.pdf
;;;

(defun make-collector (&key (initial-contents '() initial-contents-p)
                            (copy t))
  "Make a collector object into which things can be collected.

INITIAL-CONTENTS, if given it the initial contents of the object, a
list.

COPY (default true) means that the initial contents will be copied.
If it is false, then the initial contents will be destructively
modified by collection.

The implementation of collectors is unspecified, but they're obviously
just conses with a tail pointer in the cdr.  See TCONC in the IRM."
  (if initial-contents-p
      (let ((ic (if copy (copy-list initial-contents) initial-contents)))
        (cons ic (last ic)))
    (cons nil nil)))

(defun collector-contents (collector &optional (appending nil appendingp))
  "Return the contents of a collector

If APPENDING is given, append this to the collector (without copying
it) first.  APPENDING does not need to be a proper list or a list at
all: the last cons of the collector will be made to be APPENDING, and
if nothing has been collected previously APPENDING itself will be
returned.

If APPENDING is not given, then the collector can be used after this
but the returned contents will be destructively modified in that case.
If APPENDING is given the collector contents will generally be junk as
the tail pointer is not updated.

See NCONC-COLLECTOR-ONTO for a function which appends a list to a
pointer and updates the tail pointer appropriately."
  (if (not appendingp)
      (car collector)
    (if (null (cdr collector))
        appending
      (progn
        (setf (cdr (cdr collector)) appending)
        (car collector)))))

(defun collect-into (collector value)
  "Collect VALUE into COLLECTOR, returning VALUE.

If COLLECTOR is something made by MAKE-COLLECTOR, do the right thing.
If it is a function (such as the local functions defined by COLLECTING
/ WITH-COLLECTORS), simply call it with the value.

This is the closest equivalent to Interlisp's TCONC."
  (etypecase collector
    (function
     (funcall collector value))
    (cons
     (let ((it (list value)))
       (if (null (cdr collector))
           (setf (car collector) it
                 (cdr collector) it)
         (setf (cdr (cdr collector)) it
               (cdr collector) it))
       value))))

(defun nconc-collectors (collector &rest collectors)
  ;; Note unlike APPEND it makes no sense to call this with no
  ;; collectors at all: what should it return in that case (perhaps a
  ;; new collector)?
  (declare (dynamic-extent collectors))

  "Destructively concatenate one or more collectors, returning the first

All the collectors share a tail pointer after this is done, while
their head pointers point at appropriate points on the NCONCed list.
You can then collect more into any one of them but this will make the tail
pointers of all the others junk."
  (if (null collectors)
      collector
    (labels ((ncc (c a more)
               (if (null more)
                   (progn
                     (if (null (cdr c))
                         (setf (car c) (car a)
                               (cdr c) (cdr a))
                       (setf (cdr (cdr c)) (car a)
                             (cdr c) (cdr a)))
                     c)
                 (ncc c (ncc a (first more) (rest more)) '()))))
      (ncc collector (first collectors) (rest collectors)))))

(defun nconc-collector-onto (collector onto)
  "Append ONTO to the end of COLLECTOR, uodating the tail pointer

This does not copy ONTO, so it will be modified if any more elements
are collected into COLLECTOR.  Takes time proportional to the length
of ONTO.

Return the collector."
  (if (null (cdr collector))
      (setf (car collector) onto
            (cdr collector) (last onto))
    (setf (cdr (cdr collector)) onto
          (cdr collector) (last onto)))
  collector)
