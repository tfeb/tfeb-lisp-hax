;;;; Processing declarations
;;;
;;; I am not completely sure if this is a reasonable interface yet.
;;;
;;; There was formerly a macro, DO-DECLARATION-SPECIFIERS, which I
;;; decided was just not up to it, so it's gone.  Maybe a better one
;;; could be recreated.
;;;

;;; I don't want to use anything apart from utilities here (so no
;;; collecting in particular), so anything else can use this without
;;; thinking.
;;;
#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.utilities :compile t))

(defpackage :org.tfeb.hax.process-declarations
  (:use :cl)
  (:use :org.tfeb.hax.utilities)
  (:export
   #:process-declaration-identifier
   #:process-declaration-specifier))

(in-package :org.tfeb.hax.process-declarations)

(provide :org.tfeb.hax.process-declarations)

(defgeneric process-declaration-identifier (identifier function &key &allow-other-keys)
  (:documentation
   "Define a method for processing declaration specifiers whose identifer
is IDENTIFIER

Methods on this generic function should be EQL methods for specific
non-CL-standard identifiers, and should arrange to call FUNCTION
suitably, and return its values.

The function should be called with the identifier and a constructor
function, together with keyword arguments as specified by
PROCESS-DECLARATION-SPECIFIER.  Methods must:

- provide VARIABLE-NAMES or FUNCTION-NAMES lists where applicable
- add any keyword arguments & values they with
- pass all the remaining arguments to the call to the GF to the
  function
- return the values from the function as their values

They may also provide any specific keyword arguments which they should
document.

Specify methods only for non-CL-standard identifiers."))

(defun process-declaration-specifier (d f &rest args &key (environment nil) &allow-other-keys)
  "Call F for a declaration specifier D with suitable arguments

The keyword argument ENVIRONMENT can be used to specify the macro
environment object, which is used to determine type information for
TYPE and FTYPE specifiers at least.  Other keyword arguments are
passed to the function F.

F is called with two positional arguments and a number of keyword arguments.

The positional arguments are

- IDENTIFIER, the declaration identifier
- CONSTRUCTOR, a function which will construct a suitable
  declaration specifier of the same kind (see below)

Keyword arguments are
- VARIABLE-NAMES is a list of variable names for this specifier
- FUNCTION-NAMES is a list of function names for this specifier
- SPECIFIER is the whole declaration specifier
- ORIGINAL-SPECIFIER is the original specifier which may differ for
  shorthand type specifiers
- SPECIFIER-BODY is the body of the declaration specifier
- ENVIRONMENT is the lexical environment, if any

other keyword arguments come from ARGS, or may be provided by
non-standard declaration handlers.  It is generally best if the
function accepts unknown keyword arguments.

The constructor function takes keyword arguments which depend on the
identifier.  For specifiers which affect variable-names, it will take
a VARIABLE-NAMES keyword argument to allow the specification of
variables, and for ones which affect functions it will take a
FUNCTION-NAMES keyword argument.  These are the only cases for
CL-standard declarations.  See PROCESS-DECLARATION-IDENTIFIER for how
this can be extended for nonstandard declarations.

A fallback method will call F with a constructor which is a function
of no arguments which simply returns the specifier.

The return values of this function are the return values of F."
  (let ((canonical (canonicalize-declaration-specifier d environment)))
    (apply #'process-declaration-identifier (first canonical) f
           :variable-names '()
           :function-names '()
           :specifier canonical
           :original-specifier d
           :specifier-body (rest canonical)
           :environment environment
           args)))

(defmethod process-declaration-identifier ((identifier t) function
                                           &rest args &key
                                           specifier)
  (apply function
         identifier (lambda () specifier)
         args))

(defmethod process-declaration-identifier ((identifier (eql 'type)) function
                                           &rest args &key
                                           specifier-body
                                           environment)
  (destructuring-bind (type . variable-names) specifier-body
    (unless (valid-type-specifier-p type environment)
      (warn "~S is not a valid type specifier" type))
    (apply function
           identifier
           (lambda (&key (variable-names '()))
             `(type ,type ,@variable-names))
           :variable-names variable-names
           args)))

(defmethod process-declaration-identifier ((identifier (eql 'ftype)) function
                                           &rest args &key
                                           specifier-body
                                           environment)
  (destructuring-bind (ftype . function-names) specifier-body
    (unless (valid-type-specifier-p ftype environment)
      (warn "~S is not a valid type specifier" ftype))
    (apply function
           identifier
           (lambda (&key (function-names '()))
             `(ftype ,ftype ,@function-names))
           :function-names function-names
           args)))

(defun process-simple-variable-identifier (identifier function variable-names args)
  (apply function
         identifier
         (lambda (&key (variable-names '()))
           `(,identifier ,@variable-names))
         :variable-names variable-names
         args))

(defmethod process-declaration-identifier ((identifier (eql 'special)) function
                                           &rest args &key
                                           specifier-body)
  (process-simple-variable-identifier identifier function specifier-body args))

(defmethod process-declaration-identifier ((identifier (eql 'dynamic-extent)) function
                                           &rest args &key
                                           specifier-body)
  (process-simple-variable-identifier identifier function specifier-body args))

(defun process-ignore-identifier (identifier function body args)
  ;; You can ignore both functions and variables
  (apply function
         identifier
         (lambda (&key (variable-names '()) (function-names '()))
           `(,identifier ,@variable-names ,@(mapcar (lambda (f)
                                                 `(function ,f))
                                               function-names)))
         :variable-names (mapcan (lambda (e)
                              (if (symbolp e)
                                  (list e)
                                '()))
                            body)
         :function-names (mapcan (lambda (e)
                              (typecase e
                                (symbol '())
                                (cons
                                 (if (and (= (list-length e) 2)
                                          (eql (first e) 'function))
                                   (list (second e))
                                   (warn "mutant ignorable thing ~S" e)))
                                (t
                                 (warn "mutant ignorable thing ~S" e))))
                            body)
         args))

(defmethod process-declaration-identifier ((identifier (eql 'ignore)) function
                                           &rest args &key
                                           specifier-body)
  (process-ignore-identifier identifier function specifier-body args))

(defmethod process-declaration-identifier ((identifier (eql 'ignorable)) function
                                           &rest args &key
                                           specifier-body)
  (process-ignore-identifier identifier function specifier-body args))

(defun process-simple-function-identifier (identifier function function-names args)
  (apply function
         identifier
         (lambda (&key (function nil functionp) (function-names '()))
           (if functionp
               `(,identifier ,function ,@function-names)
             `(,identifier ,@function-names)))
         :function-names function-names
         args))

(defmethod process-declaration-identifier ((identifier (eql 'inline)) function
                                           &rest args &key
                                           specifier-body)
  (process-simple-function-identifier identifier function specifier-body args))

(defmethod process-declaration-identifier ((identifier (eql 'notinline)) function
                                           &rest args &key
                                           specifier-body)
  (process-simple-function-identifier identifier function specifier-body args))
