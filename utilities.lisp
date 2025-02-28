;;;; Small utilities, used mostly by other hax
;;;

(defpackage :org.tfeb.hax.utilities
  (:use :cl)
  (:export
   #:parse-docstring-body
   #:parse-simple-body
   #:with-names
   #:symbolify
   #:thunk
   #:thunk*
   #:valid-type-specifier-p
   #:canonicalize-declaration-specifier))

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
  "Parse a body that may have a docstring and declarations at the start

Return three values: the docstring, or NIL, a list of declarations and
a list of forms."
  ;; Note a docstring may be intertwined with declarations: the
  ;; previous version of this got that wrong.
  (labels ((grovel (tail docstring scled)
             (if (null tail)
                 (values docstring (nreverse scled) tail)
               (destructuring-bind (this . more) tail
                 (cond
                  ((and (not docstring) (stringp this) (not (null more)))
                   (grovel more this scled))
                  ((stringp this)
                   ;; Sanity check for extra declarations
                   (let ((next (first more)))
                     (when (and (consp next)
                                (eq (car next) 'declare))
                       (warn "unexpected declare after end of preamble")))
                   (values docstring (nreverse scled) tail))
                  ((and (consp this) (eq (first this) 'declare))
                   (grovel more docstring (cons this scled)))
                  (t
                   (values docstring (nreverse scled) tail)))))))
    (grovel doc/decls/forms nil '())))

(defun symbolify (where &rest things)
  "Make a symbol from string representations of THINGS, interning it if WHERE is given

The argument order is slightly odd, but it makes sense since you want
to be able to provide as many arguments as you need."
  (declare (dynamic-extent things))
  (if where
      (intern (apply #'concatenate 'string
                     (mapcar #'string things))
              (if (eq where t) *package* where))
      (make-symbol (apply #'concatenate 'string
                          (mapcar #'string things)))))

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

This works by asking whether TYPEP does not signal an error for it, or
by an implementation-specific function if I know one.

Yes, having to catch the error is horrible: this is a deficiency of CL."
  #+SBCL
  (sb-ext:valid-type-specifier-p thing environment)
  #-(or SBCL)
  (not (nth-value 1 (ignore-errors (typep nil thing environment)))))

(defun canonicalize-declaration-specifier (declaration &optional (environment nil))
  "Attempt to canonicalize a declaration specifier

Return two values: the canonical declaration specifier and a 'strictly
conforming' flag (see below).  If the declaration specifier is
hopeless an error is signalled.

This will turn declaration specifiers of the form (<type> ...) into
something of the form (type <type> ...) so long as it can recognise
<type> as a type.

ENVIRONMENT, if given, is used by VALID-TYPE-SPECIFIER-P and hence
SUBTYPEP and allows it to know about things which the file compiler
knows will be type specifiers when the file is loaded.

The second value is true if the declaration specifier is strictly
conforming.  That means that the identifier must be a symbol, not a
compound type specifier (see the spec for 'declaration identifier').

In the non-strictly-conforming case of a compound type specifier, the
type specifier is not checked for validity: it's just assumed to be valid."
  (etypecase declaration
    (cons
      (destructuring-bind (specifier/type . rest) declaration
       (etypecase specifier/type
         (symbol
          (values
           (case specifier/type
             ;; Catching these explicitly reduces the chance of a
             ;; calls to VALID-TYPE-SPECIFIER-P which means SBCL
             ;; generates fewer spurous compile-time warnings.
             ((declaration dynamic-extent ftype function ignore ignorable
                           inline notinline optimize special type)
              declaration)
             (otherwise
              (if (valid-type-specifier-p specifier/type environment)
               `(type ,specifier/type ,@rest)
               declaration)))
           t))
         (cons
          (values `(type ,specifier/type ,@rest) nil)))))))
