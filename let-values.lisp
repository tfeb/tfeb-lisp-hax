;;;; LET-VALUES & its friends
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.spam :compile t)
 (:org.tfeb.hax.collecting :compile t)
 (:org.tfeb.hax.iterate :compile t)
 (:org.tfeb.hax.utilities :compile t)
 (:org.tfeb.hax.process-declarations :compile t))

(defpackage :org.tfeb.hax.let-values
  (:use :cl)
  (:use
   :org.tfeb.hax.spam
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.utilities
   :org.tfeb.hax.process-declarations)
  (:export
   #:let-values
   #:let*-values
   #:let-values*
   #:let*-values*))

(in-package :org.tfeb.hax.let-values)

(provide :org.tfeb.hax.let-values)

(define-condition let-values-error (program-error simple-error)
  ())

(defun let-values-error (control &rest arguments)
  (error 'let-values-error
         :format-control control
         :format-arguments arguments))

(defun make-vme (name starred)
  (cons name (if starred name (make-symbol (symbol-name name)))))

(defun vme-name (vme)
  (car vme))

(defun vme-hidden (vme)
  (cdr vme))

(defun mapped-variable-declarations (declarations varmap environment special-too complement)
  ;; Return the appropriate variable declarations from DECLARATIONS
  ;; mapped through VARMAP.  With COMPLEMENT return the complement of
  ;; this set.  'Appropriate' means type declarations and
  ;; dynamic-extent declarations.  if SPECIAL-TOO is given also map
  ;; those.  This matters for non-final groups in sequential binding
  ;; constructs, since later initforms can refer to the special
  ;; binding.
  (multiple-value-bind (selected others)
      (with-collectors (select other)
        (dolist (declaration declarations)
          (dolist (specifier (rest declaration))
            (processing-declaration-specifier (specifier :bindings ((variable-names '()))
                                                         :identifier identifier
                                                         :constructor maker
                                                         :environment environment)
              (if (and (not (null variable-names))
                       (or special-too (not (eq identifier 'special))))
                  (multiple-value-bind (hits misses)
                      (with-collectors (hit miss)
                        (dolist (variable variable-names)
                          (let ((found (find variable varmap :key #'vme-name)))
                            (if found
                                (hit (vme-hidden found))
                              (miss variable)))))
                    (unless (null hits)
                      (select (maker :variable-names hits)))
                    (unless (null misses)
                      (other (maker :variable-names misses))))
                (other specifier))))))
    (if (not complement)
        (if (not (null selected))
            `((declare ,@selected))
          '())
      (if (not (null others))
          `((declare ,@others))
        '()))))

(defun expand-lv (clauses decls/forms starred environment &aux (unique-variables '()))
  (let ((good-clause (some-of (list-matches (list-of (var)) (any)) ;((x ...) <form>)
                              (list-matches (list-of (var)))       ;((x))
                              (list-matches (var) (any))           ;(x <form>)
                              (list-matches (var))                 ;(x)
                              (var))))
    (unless (matchp clauses (list-of good-clause))
      (if (matchp clauses (list-of (any)))
          (let ((bads (collecting
                        (dolist (clause clauses)
                          (unless (matchp clause good-clause)
                            (collect clause))))))
            (let-values-error "bad let-values clauses ~{~S~^, ~} from ~S" bads clauses))
        ;; This can't happen because of the macro definition, but.
        (let-values-error "hopeless let-values clauses ~S" clauses))))
  (if (null clauses)
      `(locally ,@decls/forms)
    (multiple-value-bind (varmaps vfs whole-varmap)
        (with-collectors (varmap vf whole-varmap-entry)
          (dolist (clause clauses)
            (varmap
             (with-collectors (vme)
               (matching clause
                 ((head-matches (list-of (var)))
                  (destructuring-bind (vars &optional (form nil)) clause
                    (vf form)
                    (dolist (var vars)
                      (unless starred
                        (when (member var unique-variables)
                          (let-values-error "Variable ~S is not unique" var))
                        (push var unique-variables))
                      (let ((vme (make-vme var starred)))
                        (vme vme) (whole-varmap-entry vme)))))
                 ((head-matches (var))
                  (destructuring-bind (var &optional (form nil)) clause
                    (vf form)
                    (unless starred
                      (when (member var unique-variables)
                        (let-values-error "Variable ~S is not unique" var))
                      (push var unique-variables))
                    (let ((vme (make-vme var starred)))
                      (vme vme) (whole-varmap-entry vme))))
                 ((var)
                  (let ((var clause))
                    (vf nil)
                    (unless starred
                      (when (member var unique-variables)
                        (let-values-error "Variable ~S is not unique" var))
                      (push var unique-variables))
                    (let ((vme (make-vme var starred)))
                      (vme vme) (whole-varmap-entry vme))))
                 (otherwise
                  (let-values-error "Oops: report this, please")))))))
      (assert (= (length varmaps) (length vfs)) () "botched") ;our fault
      (multiple-value-bind (declarations forms) (parse-simple-body decls/forms)
        (iterate mvb ((vms varmaps) (initforms vfs))
          (destructuring-bind (vm . more-vms) vms
            (destructuring-bind (initform . more-initforms) initforms
              `(multiple-value-bind ,(mapcar #'vme-hidden vm) ,initform
                 ,@(mapped-variable-declarations declarations vm environment starred nil)
                 ,@(cond
                    ((not (null more-vms))
                     `(,(mvb more-vms more-initforms)))
                    ((not starred)
                     `((let ,(mapcar (lambda (vme)
                                       `(,(vme-name vme) ,(vme-hidden vme)))
                                     whole-varmap)
                         ,@declarations
                         ,@forms)))
                    (starred
                     `(,@(mapped-variable-declarations declarations whole-varmap environment t t)
                       ,@forms)))))))))))

(defmacro let-values ((&rest clauses) &body decls/forms &environment environment)
  "Multiple-value LET form with parallel binding

LET-VALUES is an extension of LET where each clause binds a list of
variables to the multiple values of its initform.  Each binding clause
may be

 ((var ...) form) binds all the variables to the values of form
 ((var ...))      binds all variables to NIL
 (var form)       as for LET
 var              as for LET

Bindings for different clauses happen in parallel, as for LET, so

 (let ((a 1))
   (let-values (((a) 2)
                ((b) a))
     (values a b)))

evaluates to 2 and 1

Declarations should be handled correctly and perhaps usefully."
  (expand-lv clauses decls/forms nil environment))

(defmacro let*-values ((&rest clauses) &body decls/forms &environment environment)
  "Multiple-value LET form with sequential binding

LET*-VALUES is an extension of LET* where each clause binds a list of
variables to the multiple values of its initform.  Each binding clause
may be

 ((var ...) form) binds all the variables to the values of form
 ((var ...))      binds all variables to NIL
 (var form)       as for LET
 var              as for LET

Bindings for different clauses happen in sequence, as for LET*, so

 (let ((a 1))
   (let*-values (((a) 2)
                ((b) a))
     (values a b)))

evaluates to 2 and 2

Declarations should be handled correctly and perhaps usefully."
  (expand-lv clauses decls/forms t environment))

(defun expand-lv* (clauses decls/forms starred environment &aux (unique-variables '()))
  (let ((good-clause (some-of (list*-matches (list-of (var)) (any))
                              (list*-matches (var) (any)))))
    (unless (matchp clauses (list-of good-clause))
      (if (matchp clauses (list-of (any)))
          (let ((bads (collecting
                        (dolist (clause clauses)
                          (unless (matchp clause good-clause)
                            (collect clause))))))
            (let-values-error "bad let-values* clauses ~{~S~^, ~} from ~S" bads clauses))
        ;; This can't happen because of the macro definition, but.
        (let-values-error "hopeless let-values* clauses ~S" clauses))))
  (if (null clauses)
      `(locally ,@decls/forms)
    (multiple-value-bind (varmaps vis whole-varmap)
        (with-collectors (varmap vi whole-varmap-entry)
          (dolist (clause clauses)
            (varmap
             (with-collectors (vme)
               (matching clause
                 ((list*-matches (list-of (var)) (any))
                  (destructuring-bind (vars . forms) clause
                    (vi forms)
                    (dolist (var vars)
                      (unless starred
                        (when (member var unique-variables)
                          (let-values-error "Variable ~S is not unique" var))
                        (push var unique-variables))
                      (let ((vme (make-vme var starred)))
                        (vme vme) (whole-varmap-entry vme)))))
                 ((list*-matches (var) (any))
                  (destructuring-bind (var . forms) clause
                    (vi forms)
                    (unless starred
                      (when (member var unique-variables)
                        (let-values-error "Variable ~S is not unique" var))
                      (push var unique-variables))
                    (let ((vme (make-vme var starred)))
                      (vme vme) (whole-varmap-entry vme))))
                 (otherwise
                  (let-values-error "Oops: report this, please")))))))
      (assert (= (length varmaps) (length vis)) () "botched") ;our fault
      (multiple-value-bind (declarations forms) (parse-simple-body decls/forms)
        (iterate mvc ((vms varmaps) (initforms vis))
          (destructuring-bind (vm . more-vms) vms
            (destructuring-bind (this-initforms . more-initforms) initforms
              `(multiple-value-call
                   (lambda ,(mapcar #'vme-hidden vm)
                     ,@(mapped-variable-declarations declarations vm environment starred nil)
                     ,@(cond
                        ((not (null more-vms))
                         `(,(mvc more-vms more-initforms)))
                        ((not starred)
                         `((let ,(mapcar (lambda (vme)
                                           `(,(vme-name vme) ,(vme-hidden vme)))
                                         whole-varmap)
                             ,@declarations
                             ,@forms)))
                        (starred
                         `(,@(mapped-variable-declarations declarations whole-varmap environment
                                                           t t)
                           ,@forms))))
                 ,@this-initforms))))))))

(defmacro let-values* ((&rest clauses) &body decls/forms &environment environment)
    "Multiple-value LET-like form with parallel binding

Each clause in LET-VALUES* may be

 ((var ...) form ...) where there may be no forms
 (var form ...) equivalent to ((var) form ...)

Note that var on it's oen (legal for let / let-values) makes no sense
here: see below.

So each clause effectively consists of a list of variables to bind and
any number of initforms, including zero.  The variables are bound to
the combined values of all the initforms: this is the same as
MULTIPLE-VALUE-CALL, which this uses.  Example:

 (let-values* (((a b c) (values 1 2) 3))
   (values a b c))

evaluates to 1, 2 and 3

Bindings for different clauses happen in parallel, as for LET, so

 (let ((a 1))
   (let-values* (((a) 2)
                 ((b) a))
     (values a b)))

evaluates to 2 and 1.

This form is fussy about value counts which is a property of
MULTIPLE-VALUE-CALL, used in its expansion.  (let-values* (((a))) ...)
is an error, as is (let-values* (((a) 1 2)) ...).  This also means
that a single variable as in (let-values* (a ...) ...) can't ever be
valid.

Declarations should be handled correctly, and perhaps usefully"
  (expand-lv* clauses decls/forms nil environment))

(defmacro let*-values* ((&rest clauses) &body decls/forms &environment environment)
    "Multiple-value LET-like form with sequential binding

Each clause in LET-VALUES* may be

 ((var ...) form ...) where there may be no forms
 (var form ...) equivalent to ((var) form ...)

So each clause effectively consists of a list of variables to bind and
any number of initforms, including zero.  The variables are bound to
the combined values of all the initforms: this is the same as
MULTIPLE-VALUE-CALL, which this uses.  Example:

 (let-values* (((a b c) (values 1 2) 3))
   (values a b c))

evaluates to 1, 2 and 3

Bindings for different clauses happen in sequence, as for LET*, so

 (let ((a 1))
   (let-values* (((a) 2)
                 ((b) a))
     (values a b)))

evaluates to 2 and 2.

This form is fussy about value counts which is a property of
MULTIPLE-VALUE-CALL, used in its expansion.  (let-values* (((a))) ...)
is an error, as is (let-values* (((a) 1 2)) ...).  This also means
that a single variable as in (let-values* (a ...) ...) can't ever be
valid.

Declarations should be handled correctly, and perhaps usefully."
  (expand-lv* clauses decls/forms t environment))
