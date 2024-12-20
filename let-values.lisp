;;;; LET-VALUES & its friends
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.spam :compile t)
 (:org.tfeb.hax.collecting :compile t)
 (:org.tfeb.hax.iterate :compile t)
 (:org.tfeb.hax.utilities :compile t))

(defpackage :org.tfeb.hax.let-values
  (:use :cl)
  (:use
   :org.tfeb.hax.spam
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate
   :org.tfeb.hax.utilities)
  (:export
   #:let-values
   #:let*-values
   #:let-values*
   #:let*-values*))

(in-package :org.tfeb.hax.let-values)

(provide :org.tfeb.hax.let-values)

(defun make-vme (name starred)
  (cons name (if starred name (make-symbol (symbol-name name)))))

(defun vme-name (vme)
  (car vme))

(defun vme-hidden (vme)
  (cdr vme))

(defun mapped-variable-declarations (declarations varmap environment)
  ;; Appropriate variable declarations from DECLARATIONS mapped
  ;; through VARMAP 'Appropriate' means type declarations and
  ;; dynamic-extent declarations.  SPECIAL never matters (the hidden
  ;; variable can be lexical).
  (flet ((mapped-variables (varmap variables)
           (collecting
             (dolist (vme varmap)
               (when (member (vme-name vme) variables)
                 (collect (vme-hidden vme)))))))
    (collecting
      (dolist (declaration declarations)
        (dolist (specifier (mapcar (lambda (d)
                                     (canonicalize-declaration-specifier d environment))
                                   (rest declaration)))
          (destructuring-bind (identifier . rest) specifier
            (case identifier
              (type
               (destructuring-bind (type . vars) rest
                 (let ((mapped-variables (mapped-variables varmap vars)))
                   (unless (null mapped-variables)
                     (collect `(declare (type ,type ,@mapped-variables)))))))
              (dynamic-extent
               (let ((mapped-variables (mapped-variables varmap rest)))
                 (unless (null mapped-variables)
                   (collect `(declare (,identifier ,@mapped-variables)))))))))))))

(defun expand-lv (clauses decls/forms starred environment &aux (unique-variables '()))
  (unless (matchp clauses (list-of (some-of (list-matches (list-of (var)) (any))
                                            (list-matches (list-of (var))))))
    (error "bad clauses ~S" clauses))
  (if (null clauses)
      `(locally ,@decls/forms)
    (multiple-value-bind (varmaps vfs)
        (with-collectors (varmap vf)
          (dolist (clause clauses)
            (varmap
             (with-collectors (vme)
               (destructuring-bind (vars &optional (form nil)) clause
                 (vf form)
                 (dolist (var vars)
                   (unless starred
                     (when (member var unique-variables)
                       (error "~S is not unique" var))
                     (push var unique-variables))
                   (vme (make-vme var starred))))))))
      (assert (= (length varmaps) (length vfs)) () "botched")
      (let ((declarations (nth-value 0 (parse-simple-body decls/forms))))
        (iterate mvb ((vms varmaps) (forms vfs))
          (destructuring-bind (vm . more-vms) vms
            (destructuring-bind (form . more-forms) forms
              `(multiple-value-bind ,(mapcar #'vme-hidden vm) ,form
                 ,@(mapped-variable-declarations declarations vm environment)
                 ,(if (not (null more-vms))
                      (mvb more-vms more-forms)
                    `(let ,(mapcan (lambda (varmap)
                                     (mapcar (lambda (vme)
                                               `(,(vme-name vme) ,(vme-hidden vme)))
                                             varmap))
                                   varmaps)
                       ,@decls/forms))))))))))

(defmacro let-values ((&rest clauses) &body decls/forms &environment environment)
  "Multiple-value LET form with parallel binding

LET-VALUES is like LET but each clause binds a list of variables to
the multiple values of its initform.  Bindings for different clauses
happen in parallel, as for LET, so

 (let ((a 1))
   (let-values (((a) 2)
                ((b) a))
     (values a b)))

evaluates to 2 and 1

An initform may be omitted, when it will be NIL.  The equivalent of (let (a b
...) ...) is not allowed.

Declarations should be handled correctly and perhaps usefully."
  (expand-lv clauses decls/forms nil environment))

(defmacro let*-values ((&rest clauses) &body decls/forms &environment environment)
  "Multiple-value LET form with sequential binding

LET-VALUES* is like LET* but each clause binds a list of variables to
the multiple values of its initform.  Bindings for different clauses
happen in sequence, as for LET*, so

 (let ((a 1))
   (let*-values (((a) 2)
                ((b) a))
     (values a b)))

evaluates to 2 and 2

An initform may be omitted, when it will be NIL.  The equivalent of (let (a b
...) ...) is not allowed.

Declarations should be handled correctly and perhaps usefully."
  (expand-lv clauses decls/forms t environment))

(defun expand-lv* (clauses decls/forms starred environment &aux (unique-variables '()))
  (unless (matchp clauses (list-of (list*-matches (list-of (var)) (any))))
    (error "bad clauses ~S" clauses))
  (if (null clauses)
      `(locally ,@decls/forms)
    (multiple-value-bind (varmaps vis)
        (with-collectors (varmap vi)
          (dolist (clause clauses)
            (varmap
             (with-collectors (vme)
               (destructuring-bind (vars . forms) clause
                 (vi forms)
                 (dolist (var vars)
                   (unless starred
                     (when (member var unique-variables)
                       (error "~S is not unique" var))
                     (push var unique-variables))
                   (vme (make-vme var starred))))))))
      (assert (= (length varmaps) (length vis)) () "botched")
      (let ((declarations (nth-value 0 (parse-simple-body decls/forms))))
        (iterate mvc ((vms varmaps) (initforms vis))
          (destructuring-bind (vm . more-vms) vms
            (destructuring-bind (this-initforms . more-initforms) initforms
              `(multiple-value-call
                   (lambda ,(mapcar #'vme-hidden vm)
                     ,@(mapped-variable-declarations declarations vm environment)
                     ,(if (not (null more-vms))
                          (mvc more-vms more-initforms)
                        `(let ,(mapcan (lambda (varmap)
                                         (mapcar (lambda (vme)
                                                   `(,(vme-name vme) ,(vme-hidden vme)))
                                                 varmap))
                                       varmaps)
                           ,@decls/forms)))
                 ,@this-initforms))))))))

(defmacro let-values* ((&rest clauses) &body decls/forms &environment environment)
    "Multiple-value LET-like form with parallel binding

Each clause in LET-VALUES* consists of a list of variables to bind and
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

There may be no initforms which binds all variables to NIL. The
equivalent of (let (a b ...) ...)  is not supported.

Declarations should be handled correctly, and perhaps usefully"
  (expand-lv* clauses decls/forms nil environment))

(defmacro let*-values* ((&rest clauses) &body decls/forms &environment environment)
    "Multiple-value LET-like form with sequential binding

Each clause in LET*-VALUES* consists of a list of variables to bind and
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

There may be no initforms which binds all variables to NIL. The
equivalent of (let (a b ...) ...)  is not supported.

Declarations should be handled correctly, and perhaps usefully."
  (expand-lv* clauses decls/forms t environment))
