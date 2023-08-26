;;;; Some simple loops, useful with collecing
;;;

;;; Copyright 2021, 2022 by me, Tim Bradshaw, and may be used for any
;;; purpose whatsoever by anyone. It has no warranty whatsoever. I
;;; would appreciate acknowledgement if you use it in anger, and I
;;; would also very much appreciate any feedback or bug fixes.
;;;

;;; These are called PASSING &c because WHILE has the obvious syntax
;;; (while test ...) and these are (passing ((var ...) ...) ...).  The
;;; name means 'while these bindings pass do ...' .  ESCAPING is then
;;; named by analogy (it should perhaps be WITH-ESCAPE).
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate
 :org.tfeb.hax.utilities)

(defpackage :org.tfeb.hax.simple-loops
  (:use :cl)
  (:use
   :org.tfeb.hax.collecting :org.tfeb.hax.iterate
   :org.tfeb.hax.utilities)
  (:export
   #:doing #:doing*
   #:dolists
   #:passing #:passing*
   #:do-passing #:do-passing*
   #:failing #:failing*
   #:do-failing #:do-failing*
   #:looping #:looping*
   #:looping/values #:looping/values*
   #:escaping))

(in-package :org.tfeb.hax.simple-loops)

(provide :org.tfeb.hax.simple-loops)

(defun parse-clauses (clauses)
  (with-collectors (var init stepper)
    (dolist (clause clauses)
      (typecase clause
        (symbol
         (var clause) (init 'nil) (stepper clause))
        (cons
         (case (length clause)
           ((1 2)
            (var (first clause))
            (init (second clause))
            (stepper (second clause)))
           (3
            (var (first clause))
            (init (second clause))
            (stepper (third clause)))
           (otherwise
            (error "bad clause ~S" clause))))
        (t
         (error "mutant clause ~S" clause))))))

(defun expand-doing (clauses test values forms &key (sequential nil))
  (multiple-value-bind (vars inits steppers) (parse-clauses clauses)
    `(,(if sequential 'do* 'do)
      ,(mapcar #'list vars inits steppers)
      (,test ,(case (length values)
                (0
                 `(values ,@vars))
                (1
                 (first values))
                (otherwise
                `(multiple-value-call #'values ,@values))))
      ,@forms)))

(defmacro doing ((&rest clauses) (test &rest values) &body forms)
  "A variant of DO

This is like DO, but each variable binding may be one of

- <var> which binds the variable to NIL and steps it to its current
  value;
- (<var> <init/step>) which binds to value of <init/step> and steps to
  value of init/step;
- (<var> <init> <step>) which binds to <init> and steps to value of
  <step>.

If no value forms are specified then the current values of the
bindings are returned as multiple values.  If more than one value form
is specified then the combined multiple values of all of them are
returned.

This expands into DO, so all the things that are true of DO are true
here: the body is a TAGBODY, and there is a block named NIL wrapped
around the form so (RETURN ...) will work."
  (expand-doing clauses test values forms))

(defmacro doing* ((&rest clauses) (test &rest values) &body forms)
  "A variant of DO*: see DOING"
  (expand-doing clauses test values forms :sequential t))

#+LispWorks
(progn
  ;; DOING and DOING* need special rules to indent the test/return
  ;; clause under the bindings clauses
  (editor:setup-indent "doing" 2 2 7)
  (editor:setup-indent "doing*" 2 2 8))

(defun expand-simple-loop (clauses forms &key (sequential nil) (test-at-end nil)
                                   (negated nil))
  (multiple-value-bind (vars inits steppers) (parse-clauses clauses)
    (let ((do (if sequential 'do* 'do))
          (test (if negated `(and ,@vars) `(not (and ,@vars))))
          (do-clauses (mapcar #'list vars inits steppers)))
      (if test-at-end
          `(,do ,do-clauses
                ((progn
                  ,@forms
                  ,test)
                 (values ,@vars)))
        `(,do ,do-clauses
              (,test (values ,@vars))
              ,@forms)))))

(defmacro passing ((&rest clauses) &body forms)
  "Loop while some variables are all true

Each clause in CLAUSES is either <var> which is initialised to NIL and
stepped to its current value, (<var> <init/step>) which initialises
<var> to the value of <init/step> and steps it to the current value of
the same form, or (<var> <init> <step>) which splits the two forms.
The variables are bound, in parallel, to the value of the
initialisation forms, and the forms in the body are evaluated while
all the variables remain true, with the variables being updated at the
end of the body.  Once one or variables becomes false the loop stops
and the values of the variables (at least one of which will be NIL)
are returned.

This expands into DO, so all the things that are true of DO are true
here: the body is a TAGBODY, and there is a block named NIL wrapped
around the form so (RETURN ...) will work."
  (expand-simple-loop clauses forms))

(defmacro passing* ((&rest clauses) &body forms)
  "Like PASSING, but with bindings in series"
  (expand-simple-loop clauses forms :sequential t))

(defmacro do-passing ((&rest clauses) &body forms)
  "Like PASSING, but the test is at the end"
  (expand-simple-loop clauses forms :test-at-end t))

(defmacro do-passing* ((&rest clauses) &body forms)
  "LIKE PASSING but bindings are in series and the test is at the end"
  (expand-simple-loop clauses forms :test-at-end t :sequential t))

(defmacro failing ((&rest clauses) &body forms)
  "Like PASSING but loop until all the variables become true"
  (expand-simple-loop clauses forms :negated t))

(defmacro failing* ((&rest clauses) &body forms)
  "Like PASSING, but test is inverted and sequential binding"
  (expand-simple-loop clauses forms :negated t :sequential t))

(defmacro do-failing ((&rest clauses) &body forms)
  "Like PASSING but test is inverted and at the end"
  (expand-simple-loop clauses forms :negated t :test-at-end t))

(defmacro do-failing* ((&rest clauses) &body forms)
  "Like PASSING but test is inverted and at the end, and binding is sequential"
  (expand-simple-loop clauses forms :negated t :test-at-end t :sequential t))

(defun expand-looping (clauses decls/forms &key (sequential nil))
  (let ((variables (mapcar (lambda (clause)
                             (typecase clause
                               (symbol clause)
                               (cons
                                (case (length clause)
                                  ((1 2) (first clause))
                                  (otherwise (error "bad clause ~S" clause))))
                               (t (error "mutant clause ~S" clause))))
                           clauses)))
    (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
      (let ((<start> (make-symbol "START")))
        `(,(if sequential 'let* 'let) ,clauses
           ,@decls
           (block nil
             (tagbody
              ,<start>
              ,(if (= (length variables) 1)
                   `(setq ,(first variables) (progn ,@forms))
                 `(multiple-value-setq ,variables (progn ,@forms)))
              (go ,<start>))))))))

(defmacro looping ((&rest clauses) &body decls/forms)
  "A simple loop construct

Each clause in CLAUSES may either be <var> which is bound to NIL, or
may be (<var> <init>) which binds <var> to <init>.  Each time through
the loop the variables are updated by the values of the BODY.  The
values of the last of the forms in DECLS/FORMS are used to update the
variables.  There is no termination condition, but the forms are
wrapped in a block named NIL in the usual way.  Initial variable
bindings are in parallel."
  (expand-looping clauses decls/forms))

(defmacro looping* ((&rest clauses) &body decls/forms)
  "Like LOOPING but initial bindings are sequential"
  (expand-looping clauses decls/forms :sequential t))

(defmacro looping/values* ((&rest clauses) &body decls/forms)
  "A nested variant of LOOPING/VALUES

Each clause in CLAUSES is of the form either ((<var> ...) <form>), in
which case the <var>s will be bound to the multiple values of <form>,
or ((<var> ...) <form> ...) which is the same as ((<var>)
(multiple-value-call #'values <form> ...)). Each clause happens
sequentially, within the scope of bindings established by previous
clauses.  The total set of the variables is then updated by the values
of the last form in the body."
  (unless (every (lambda (clause)
                   (and (listp clause)
                        (>= (length clause) 1)
                        (listp (first clause))
                        (every #'symbolp (first clause))))
                 clauses)
    (error "bad clauses ~S" clauses))
  (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
    (let ((<start> (make-symbol "START"))
          (all-variables (mapcan #'copy-list (mapcar #'first clauses))))
      (if (null clauses)
          `(locally
             ,@decls
             (block nil
               (tagbody
                ,<start>
                (progn ,@forms)
                (go ,<start>))))
        (iterate next-clause ((clause (first clauses))
                              (more-clauses (rest clauses)))
          (destructuring-bind (variables &rest form/s) clause
            (if (null more-clauses)
                `(multiple-value-bind ,variables ,(if (= (length form/s) 1)
                                                      (first form/s)
                                                    `(multiple-value-call
                                                         #'values ,@form/s))
                     ,@decls
                     (block nil
                       (tagbody
                        ,<start>
                        (multiple-value-setq ,all-variables
                            (progn ,@forms))
                        (go ,<start>))))
                `(multiple-value-bind ,variables ,(if (= (length form/s) 1)
                                                      (first form/s)
                                                    `(multiple-value-call
                                                         #'values ,@form/s))
                   ,(next-clause (first more-clauses)
                                 (rest more-clauses))))))))))

(defmacro looping/values (((&rest variables) &rest form/s) &body decls/body)
  "Like LOOPING but with multiple values

(looping/values ((<var> ...) <form>) ...) binds the <var>s to the
values of <form> and hhen updates them with the values of the last
form in the body.  (looping/values ((<var> ...) <form> ...) binds the
<var>s to the cobined values of all the <form>'s and then updates them
with the values of the last form in the body.  As with LOOPING there
is no termination condition but the body is wrapped in a block named
NIL so RETURN will work."
  `(looping/values* ((,variables ,@form/s)) ,@decls/body))

(defmacro dolists (bindings &body decls/forms)
  "Like DOLIST but with multiple lists

Each BINDING is either (var init) or (var init value).  As many values
are returned as bindings with value forms, so this may return between
no values and as manu values as bindings.  The inits are evaluated in
parallel (nothing else really makes much sense). The loop ends when
the first list terminates. At least one the of variables will
therefore be NIL when the loop terminates (same as for DOLIST, whee
the only variable is NIL when the loop terminates)."
  ;; This was briefly done with DOING, but this seems nicer.  Note
  ;; this implementetion rebinds the variables on each iteration.
  (multiple-value-bind (vars tail-vars inits values)
      (with-collectors (var tail-var init value)
        (dolist (binding bindings)
          (typecase binding
            (cons
             (let ((l (list-length binding)))
               (unless (and (<= 2 l 3)
                            (symbolp (first binding)))
                 (error "binding ~S is malformed" binding))
               (var (first binding))
               (init (second binding))
               (tail-var (make-symbol (concatenate 'string (string (first binding))
                                                   "-TAIL")))
               (when (= l 3)
                 (value (third binding)))))
            (t
             (error "hopeless binding ~S" binding)))))
    (multiple-value-bind (decls forms) (parse-simple-body decls/forms)
    `(looping ,(mapcar #'list tail-vars inits)
       (let ,(mapcar (lambda (var tail-var)
                      `(,var (car ,tail-var)))
                    vars tail-vars)
         ,@decls
         (when (or ,@(mapcar (lambda (tail)
                               `(null ,tail))
                             tail-vars))
           (return (values ,@values)))
         ,@forms
         (values ,@(mapcar (lambda (tail-var)
                             `(cdr ,tail-var))
                           tail-vars)))))))

(defmacro escaping ((escape &rest defaults) &body forms)
  "Bind a local escape function

Within the body of this form, ESCAPE will immediately return from it.
Given arguments it returns those as multiple values.  With no
arguments it returns the values of DEFAULTS, or no values if none are
given.  The forms in DEFAULTS are not evaluated until and if the
escape function is called with no arguments, and are evaluated in the
lexical contxt of the ESCAPING form, and the dynamic context of the
point the escape function is called.

It won't work to call the escape function once control has left
ESCAPING."
  (let ((<block> (make-symbol (symbol-name escape))))
    `(block ,<block>
       (flet ((,escape (&rest args)
                (declare (dynamic-extent args))
                (return-from ,<block>
                  (if (null args)
                      (values ,@defaults)
                    (values-list args)))))
         (declare (inline ,escape)
                  (dynamic-extent (function ,escape)))
         ,@forms))))
