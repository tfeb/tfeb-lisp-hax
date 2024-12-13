;;;; Small utilities, used mostly by other hax
;;;

(defpackage :org.tfeb.hax.utilities
  (:use :cl)
  (:export
   #:parse-docstring-body
   #:parse-simple-body
   #:with-names
   #:thunk
   #:thunk*
   #:valid-type-specifier-p))

(in-package :org.tfeb.hax.utilities)

(provide :org.tfeb.hax.utilities)

(defun parse-simple-body (decls/forms)
  "Parse a body which can not have a docstring

Return two values: a list of declarations and a list of forms"
  (do* ((slced '())
        (tail decls/forms (rest tail))
        (this (first tail) (first tail)))
       ((or (null tail) (not (and (consp this)
                                  (eql (first this) 'declare))))
        (values (nreverse slced) tail))
    (push this slced)))

(defun parse-docstring-body (doc/decls/forms)
  "Parse a body that may have a docstring at the start

Return three values: the docstring, or NIL, a list of declarations and
a list of forms."
  (if (and (stringp (first doc/decls/forms))
           (not (null (rest doc/decls/forms))))
      (multiple-value-bind (decls forms)
          (parse-simple-body (rest doc/decls/forms))
        (values (first doc/decls/forms) decls forms))
      (multiple-value-bind (decls forms)
          (parse-simple-body doc/decls/forms)
        (values nil decls forms))))

(defmacro with-names ((&rest clauses) &body forms)
  "Bind a bunch of variables to fresh symbols with the same name

Optionally you can specify the name by giving a clause as (var <string-designator>)."
  `(let ,(mapcar (lambda (clause)
                   (etypecase clause
                     (symbol
                      `(,clause (make-symbol ,(string clause))))
                     (cons
                      (destructuring-bind (name sd) clause
                        `(,name (make-symbol (string ,sd)))))))
                 clauses)
     ,@forms))

;;; Because it's time
;;;

(defmacro thunk (&body body)
  "Function of no arguments"
  `(lambda ()
     ,@body))

(defmacro thunk* (&body body)
  ;; Can't use WITH-NAMES yet
  "Function of any number of ignored arguments"
  (let ((<args> (make-symbol "<ARGS>")))
    `(lambda (&rest ,<args>)
       (declare (dynamic-extent ,<args>)
                (ignore ,<args>))
       ,@body)))

(defun valid-type-specifier-p (thing &optional (environment nil))
  "Is THING a valid type specifier in ENVIRONMENT?

This works by using SUBTYPEP and catching the error, and therefore
assumes that any type specifier is a recognizable subtype of T.  If
that's not true it will perhaps fail.

Yes, having to catch the error is horrible: this is a deficiency of CL."
 (values (ignore-errors (subtypep thing t environment))))
