;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - collecting.lisp
;; Description       - Collecting lists forwards
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - 1989
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Collecting lists forwards, and other accumulation macros
;;;

;;; The original collecting is an old macro cleaned up a bit
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
;;; 1989-2012, 2021-2025 by me, Tim Bradshaw, and may be used for any
;;; purpose whatsoever by anyone. There is no warranty whatsoever. I
;;; would appreciate acknowledgement if you use this in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.utilities :compile t))

(defpackage :org.tfeb.hax.collecting
  (:use :cl)
  (:use :org.tfeb.hax.utilities)
  (:export #:collecting
          #:collect
          #:with-collectors
          #:collecting-values
          #:with-accumulators
          #:make-collector
          #:collector-contents
          #:collect-into
          #:nconc-collectors
          #:nconc-collector-onto
          #:pop-collector
          #:collector-empty-p
          #:with-vector-accumulators))

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
  ;; multiple-collector version of COLLECTING
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
    `(let ,(mapcar (lambda (cvn)
                     `(,cvn (list nil)))
                   cvns)
       (declare (type list ,@cvns))
       (let ,(mapcar #'list ctns cvns)
         (declare (type list ,@ctns))
         (flet ,(mapcar (lambda (cn ctn)
                        `(,cn (it)
                              (setf ,ctn (push it (cdr (the cons ,ctn))))
                              it))
                        collectors ctns)
         (declare (inline ,@collectors))
         ,@forms)
       (values ,@(mapcar (lambda (cvn)
                           `(cdr ,cvn))
                         cvns))))))

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
returner default by).  In this case name, operator & initially mean
exactly the same as previously, but type is a type specification for
the variable which underlies the accumulator, and returner denotes a
function of one argument, the final value of the accumulator, whose
return value is used instead of the final value.  If default is given
then the local function takes an optional argument whose default it
is.  If by is given then it takes no arguments and this is the
increment.  These two arguments are mutually exclusive.

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
                   `(name ,name on ,on init ,init arglist (it))))
                (otherwise
                 (destructuring-bind (name on &key
                                           (initially `(,on))
                                           (type nil typep)
                                           (returner nil returnerp)
                                           (default nil defaultp)
                                           (by nil byp)) a
                   (unless (symbolp name)
                     (error "the name of accumulator ~S isn't a symbol" a))
                   (unless (or (symbolp on)
                               (and (consp on)
                                    (eql (first on) 'lambda)))
                     (error "the operator of accumulator ~S~
isn't a symbol or lambda expression" a))
                   (when returnerp
                     (unless (or (symbolp returner)
                                 (and (consp returner)
                                      (eql (first returner) 'lambda)))
                       (error "the return operator of accumulator ~S~
isn't a symbol of lambda expression" a)))
                   (when (and defaultp byp)
                     (error "default and by can't both be given in ~S" a))
                   (let ((p `(name ,name on ,on init ,initially
                                   arglist ,(cond
                                             (byp `(&aux (it ,by)))
                                             (defaultp `(&optional (it ,default)))
                                             (t '(it))))))
                     (when typep
                       (setf (getf p 'type) type))
                     (when returnerp
                       (setf (getf p 'returner) returner))
                     p)))))
             (t
              (error "hopeless accumulator ~S" a))))
         (getp (plist p)
           (let* ((d (load-time-value (cons nil nil)))
                  (v (getf plist p d)))
                   (if (eq v d)
                       (values nil nil)
                     (values v t)))))
    (let* ((parsed (mapcar #'parse-accumulator accumulators))
           (vars (mapcar (lambda (p)
                            (make-symbol (symbol-name (getf p 'name))))
                          parsed)))
      `(let ,(mapcar (lambda (p v)
                       `(,v ,(getp p 'init)))
                     parsed vars)
         ,@(mapcan (lambda (p v)
                     (multiple-value-bind (type typep) (getp p 'type)
                       (if typep `((declare (type ,type ,v))) '())))
                   parsed vars)
         (flet ,(mapcar (lambda (p v)
                          (let ((arglist (getp p 'arglist))
                                (on (getp p 'on))
                                (name (getp p 'name)))
                            `(,name ,arglist (setf ,v (,on ,v it)) it)))
                        parsed vars)
           (declare (inline ,@(mapcar (lambda (p)
                                        (getp p 'name))
                                      parsed)))
           ,@forms)
         (values ,@(mapcar (lambda (p v)
                             (multiple-value-bind (returner returnerp) (getp p 'returner)
                               (if returnerp
                                   `(,returner ,v)
                                 v)))
                           parsed vars))))))

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
  "Destructively concatenate one or more collectors, returning the first

All the collectors share a tail pointer after this is done, while
their head pointers point at appropriate points on the NCONCed list.
You can then collect more into any one of them but this will make the tail
pointers of all the others junk."
  (declare (dynamic-extent collectors))
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

(defun pop-collector (c)
  "Pop the first value from C

If C is empty, then this will return NIL"
  (prog1 (pop (car c))
    (when (null (car c))
      (setf (cdr c) nil))))

(defun collector-empty-p (c)
  "Return true if the collector C is empty"
  (null (car c)))

;;;; Vector accumulators (try)
;;;
;;; This doesn't use org.tfeb.hax.iterate to avoid dragging in things,
;;; and can't use org.tfeb.dsm because that would be circular.
;;;
;;; It's also much hairier than any of the other macros here: perhaps
;;; it should live somewhere else?
;;;

(defconstant default-initial-va-length 8)
(defconstant default-initial-va-extension 1.5)

(defun vector-accumulator-description (fname kws environment)
  ;; Return four values:
  ;; - a list of names of variables for the vector, its initial
  ;;   length, start position / index, whether it has a fill pointer,
  ;;   whether it is adjustable, the extension factor and whether this
  ;;   was given;
  ;; - a form which will return initial values for these variables;
  ;; - a declaration form
  ;; - a list of the arglist & body of a function which will
  ;;   accumulate an element into the vector
  (destructuring-bind (&key (for-vector nil)
                            (start 0 startp)
                            (fill-pointer nil)
                            (adjustable nil)
                            (length default-initial-va-length ilp)
                            (extension default-initial-va-extension extp)
                            (element-type (if for-vector '* t)) ;see below
                            (finalize t)
                            (simple nil))
      kws
    (with-names ((<v> (stringify "<" fname "-V>"))
                 (<l> (stringify "<" fname "-L>"))
                 (<s> (stringify "<" fname "-S>"))
                 (<fpp> (stringify "<" fname "-FPP>"))
                 (<adjp> (stringify "<" fname "-ADJP>"))
                 (<ext> (stringify "<" fname "-EXT>"))
                 (<extp> (stringify "<" fname "-EXTP>"))
                 (<finalize> (stringify "<" fname "-FINALIZE>")))
      (when (and for-vector (or fill-pointer adjustable ilp))
        (warn "Shouldn't specify characteristics of provided vector"))
      (multiple-value-bind (literal-et et-literal-p)
          (typecase element-type
            (boolean (values element-type t))
            (cons
             (if (eq (first element-type) 'quote)
                 (values (second element-type) t)
               (values nil nil)))
            (t (values nil nil)))
        (values
         ;; Variables
         (list <v> <l> <s> <fpp> <adjp> <ext> <extp> <finalize>)
         ;; Initialization form
         (if for-vector
             ;; We are given the vector
             `(let* ((,<v> ,for-vector)
                     (,<fpp> (array-has-fill-pointer-p ,<v>)))
                (values
                 ,<v>
                 (length ,<v>)
                 ,(if startp
                      `(if ,<fpp>
                           (setf (fill-pointer ,<v>) ,start)
                         ,start)
                    `(if ,<fpp>
                         (fill-pointer ,<v>)
                       ,start))
                 ,<fpp>
                 (adjustable-array-p ,<v>)
                 (let ((ext ,extension))
                   (typecase ext
                     (real ext)
                     (function ext)
                     (symbol (symbol-function ext))
                     (t (error "Extension can't be a ~S" (type-of ext)))))
                 ,extp
                 ,finalize))
           ;; We are making the vector
           (with-names ((<fp> (stringify "<" fname "-FP>")))
             `(let* ((,<fp> ,fill-pointer)
                     (,<adjp> ,adjustable)
                     (,<l> ,length)
                     (,<v> (make-array ,<l>
                                       :element-type ,element-type
                                       :fill-pointer ,<fp>
                                       :adjustable ,<adjp>)))
                (values
                 ,<v>
                 ,<l>
                 ,(if startp
                      `(if ,<fp>
                           (setf (fill-pointer ,<v>) ,start)
                         ,start)
                    `(if ,<fp> (fill-pointer ,<v>) ,start))
                 ,<fp>
                 ,<adjp>
                 (let ((ext ,extension))
                   (typecase ext
                     (real ext)
                     (function ext)
                     (symbol (symbol-function ext))
                     (t (error "Extension can't be a ~S" (type-of ext)))))
                 ,extp
                 ,finalize))))
         ;; Declaration
         `(declare
           (type (integer 0 ,array-dimension-limit) ,<l>)
           (type (integer 0 (,array-dimension-limit)) ,<s>)
           ;; Trust literal element types, otherwise assume *. The
           ;; array is certainly simple if we made it and there is
           ;; certainly no fill pointer or adjustability
           ,(let ((et (if et-literal-p literal-et '*))
                   (ay (if (or (and (not for-vector)
                                    (not (or fill-pointer adjustable)))
                               (and for-vector
                                    (constantp simple environment)
                                    simple))
                           'simple-array 'array)))
              `(type (,ay ,et (*)) ,<v>))
           ,@(if (not (or for-vector fill-pointer adjustable))
                 ;; We'll hit the simple case, and these are unused
                 `((ignore ,<adjp> ,<extp>))
               '()))
         ;; Accumulating function definition
         (if (not (or for-vector fill-pointer adjustable))
             ;; Simple, common case.  We could handle intermediate cases but
             ;; why bother?
             `((e)
               ,@(if et-literal-p
                     `((declare (type ,literal-et e)))
                   '())
               (when (>= ,<s> ,<l>)
                 ;; Must extend
                 (setf ,<l> (typecase ,<ext>
                              (real (max (1+ ,<l>) (round (* ,<l> ,<ext>))))
                              (function (funcall ,<ext> ,<l>)))
                       ,<v> (adjust-array ,<v> ,<l>)))
               (setf (aref ,<v> ,<s>) e)
               (incf ,<s>)
               e)
           ;; General case
           `((e)
             ,@(if et-literal-p
                   `((declare (type ,literal-et e)))
                 '())
             (cond
              (,<fpp>
               ;; fill pointer
               (cond
                (,<adjp>
                 (if ,<extp>
                     (vector-push-extend e ,<v>
                                         (- ,<l> (typecase ,<ext>
                                                   (real (max (1+ ,<l>) (round (* ,<l> ,<ext>))))
                                                   (function (funcall ,<ext> ,<l>)))))
                   (vector-push-extend e ,<v>))
                 (setf ,<l> (length ,<v>)))
                ((>= ,<s> ,<l>)
                 (setf ,<l> (typecase ,<ext>
                              (real (max (1+ ,<l>) (round (* ,<l> ,<ext>))))
                              (function (funcall ,<ext> ,<l>)))
                       ,<v> (adjust-array ,<v> ,<l>))
                 (vector-push e ,<v>))
                (t
                 (vector-push e ,<v>))))
              ((>= ,<s> ,<l>)
               ;; No fill pointer, past end
               (setf ,<l> (typecase ,<ext>
                            (real (max (1+ ,<l>) (round (* ,<l> ,<ext>))))
                            (function (funcall ,<ext> ,<l>)))
                     ,<v> (adjust-array ,<v> ,<l>)
                     (aref ,<v> ,<s>) e))
              (t
               ;; No fill pointer, not past end
               (setf (aref ,<v> ,<s>) e)))
             (incf ,<s>)
             e)))))))

(defmacro with-vector-accumulators ((&rest accumulators) &body decls/forms
                                    &environment environment)
  ;; The macro.  This is too hairy and should perhaps be factored out
  ;; into some kind of expansion function
  (if (null accumulators)
      ;; Trivial case
      `(locally ,@decls/forms)
    ;; Real case
    (multiple-value-bind (varlists initforms decls fnames fdescs)
        (with-collectors (varlist initform decl fname fdesc)
          (dolist (accumulator accumulators)
            (destructuring-bind (fname &rest kws &key &allow-other-keys)
                (etypecase accumulator
                  (list
                   (if (evenp (length accumulator))
                       (destructuring-bind (n v &rest kws &key &allow-other-keys)
                           accumulator
                         `(,n :for-vector ,v ,@kws))
                     accumulator))
                  (symbol (list accumulator)))
              (multiple-value-bind (varlist initform decl fdesc)
                  (vector-accumulator-description fname kws environment)
                (varlist varlist)
                (initform initform)
                (decl decl)
                (fname fname)
                (fdesc fdesc)))))
      (labels ((write-bindings (vls-tail ifs-tail decls-tail)
                 (if (not (null vls-tail))
                     `(multiple-value-bind ,(first vls-tail) ,(first ifs-tail)
                        ,(first decls-tail)
                        ,(write-bindings (rest vls-tail) (rest ifs-tail) (rest decls-tail)))
                   ;; All done
                   `(flet ,(mapcar #'cons fnames fdescs)
                      (declare (inline ,@fnames))
                      ,@decls/forms
                      ;; Value-returning form can be hairy
                      ,(multiple-value-bind (rvs lvs svs fppvs fzvs)
                           (with-collectors (rv lv sv fppv fzv)
                             (dolist (varlist varlists)
                               (destructuring-bind (rv lv sv fppv adjpv
                                                       extv extpv fzv) varlist
                                 (declare (ignore adjpv extv extpv))
                                 (rv rv) (lv lv) (sv sv) (fppv fppv) (fzv fzv))))
                         `(values ,@(mapcar
                                     (lambda (rv lv sv fppv fzv)
                                       `(if ,fzv
                                            (cond
                                             ((= ,lv ,sv)
                                              ,rv)
                                             (,fppv
                                              ;; Prefer changing fp
                                              ;; if we can
                                              (setf (fill-pointer ,rv) ,sv)
                                              ,rv)
                                             (t
                                              (adjust-array ,rv ,sv)))
                                          ,rv))
                                     rvs lvs svs fppvs fzvs)))))))
        (write-bindings varlists initforms decls)))))
