;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File             - trace-macroexpand.lisp
;; Description      - Macroexpansion tracing
;; Author           - Tim Bradshaw (tfb at kingston.local)
;; Created On       - Fri Dec 13 11:35:01 2019
;; Status           - Unknown
;;
;; $Format:(@:%H)$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(eval-when (:load-toplevel :compile-toplevel :execute)
  ;; macroexpansion tracing really wants to be off when compiling this
  ;; code as exciting things may happen during the evaluation of
  ;; DEFVAR &c otherwise.
  (let ((function (and (find-package :org.tfeb.hax.trace-macroexpand)
                       (find-symbol (symbol-name '#:trace-macroexpand)
                                    :org.tfeb.hax.trace-macroexpand))))
    (when (and function (fboundp function))
      (ignore-errors                      ;don't barf
        (funcall function nil)))))

(defpackage :org.tfeb.hax.trace-macroexpand
  (:use :cl)
  (:export
   #:*trace-macroexpand-print-length*
   #:*trace-macroexpand-print-level*
   #:*trace-macroexpand-maybe-trace*
   #:*trace-macroexpand-trace-hook*
   #:*trace-macroexpand-traced-packages*
   #:*trace-macroexpand-traced-names*
   #:*trace-macroexpand-printer*
   #:trace-macroexpand
   #:macroexpand-traced-p
   #:call/macroexpand-tracing
   #:with-macroexpand-tracing
   #:trace-macro
   #:untrace-macro
   #:trace-macro-package
   #:untrace-macro-package))

(in-package :org.tfeb.hax.trace-macroexpand)

(provide :org.tfeb.hax.trace-macroexpand)

(defvar *trace-macroexpand-print-length* 3
  "The value of *PRINT-LENGTH* used when tracing macroexpansions")

(defvar *trace-macroexpand-print-level* 2
  "The value of *PRINT-LEVEL* used when tracing macroexpansions")

(defvar *trace-macroexpand-maybe-trace* t
  "Should we even consider tracing?

If this is false then don't trace, at all.  Overrides everything else.")

(defvar *trace-macroexpand-traced-packages* '()
  "A list of package designators in which macros should be traced.

Macros whose names are accessible in packages specified by this list
are traced.  Each element is either: a package, a package name, NIL or
T.  NIL means 'this package', T means 'all packages'.

This list will canonicalised to contain either strings, T or NIL by
TRACE-MACRO-PACKAGE and UNTRACE-MACRO-PACKAGE.

Macros are traced as specified by this list and
*TRACE-MACROEXPAD-TRACED-NAMES*: tracing happens if the macros'
package matches this list, or its name is in
*TRACE-MACROEXPAD-TRACED-NAMES*.  This means that to trace only
specific macros you want this list to be empty.

This mechanism can be overridden by *TRACE-MACROEXPAND-TRACE-HOOK* &
*TRACE-MACROEXPAND-MAYBE-TRACE*")

(defvar *trace-macroexpand-traced-names* '()
  "A list of macro names to trace.

If a macro's name is on this list, then it's traced.

Macros are traced as specified by this list and
*TRACE-MACROEXPAND-TRACED-PACKAGES*: macros are traced if they are on
this list or if their package matches
*TRACE-MACROEXPAND-TRACED-PACKAGES*.  This means that to trace only
specific macros you want *TRACE-MACROEXPAND-TRACED-PACKAGES* to be
empty, while to trace all macros visible in a package you want to use
*TRACE-MACROEXPAND-TRACED-PACKAGES*.

This mechanism can be overridden by *TRACE-MACROEXPAND-TRACE-HOOK* &
*TRACE-MACROEXPAND-MAYBE-TRACE*")

(defvar *trace-macroexpand-trace-hook* nil
  "If not NIL a function which determines whether a macro is traced.

If this variable is not NIL it should be bound to a function
designator which determines if a given macro is traced.  This function
completely replaces the built-in mechanism which uses
*TRACE-MACROEXPAND-TRACED-PACKAGES* and
*TRACE-MACROEXPAD-TRACED-NAMES*.  The function is called with the
macro function, the form being expanded and the environment (the same
arguments as the function bound to *MACROEXPAND-HOOK*, which see).
The macro is traced if it returns true.

This mechanism can be overridden by *TRACE-MACROEXPAND-MAYBE-TRACE*")

(defun trace-macroexpand-trace-p (macro-function macro-form environment)
  ;; Determine if a macro should be traced using the above variables.
  (cond ((not *trace-macroexpand-maybe-trace*)
         ;; Not even thinking about it
         nil)
        (*trace-macroexpand-trace-hook*
         ;; User hook decides unilaterally
         (funcall *trace-macroexpand-trace-hook*
                  macro-function macro-form environment))
        ((and (consp macro-form)
              (symbolp (first macro-form)))
         (let* ((macro-name (first macro-form))
                (macro-package (symbol-package macro-name)))
           (or
            (dolist (designator *trace-macroexpand-traced-packages* nil)
              ;; Package rules: I found LOOP just more confusing than
              ;; this, but this is fairly confusing
              (cond
               ((eql designator 't)
                ;; trace all packages
                (return t))
               ((and (eql designator 'nil)
                     (eql (find-symbol (symbol-name macro-name) *package*)
                          macro-name))
                ;; trace symbols visible in the current package
                (return t))
               ((or (stringp designator)
                    (symbolp designator))
                ;; trace if packages are the same, but only when there
                ;; is a package to avoid uninterned things
                (let ((designator-package (find-package designator)))
                  (when (and designator-package
                             (eql designator-package macro-package))
                    (return t))))
               (t
                ;; Something illegal: don't trace, but don't blow up
                (warn "bogus elt ~A in ~S" designator
                      '*trace-macroexpand-traced-packages*))))
            ;; name rules are much simpler!
            (member macro-name *trace-macroexpand-traced-names*))))
        (t
         ;; No idea what this is, but it may be legal: don't trace it
         nil)))

(defvar *trace-macroexpand-printer* nil
  "Printer for traced macro expansions

If this is not NIL it should be a designator for a function of four
arguments: the stream to print on, the macro form, the expanded
form and the environment.

If this is not NIL then the function is called without any
locally-bound values for printer-control variables, so
*TRACE-MACROEXPAND-PRINT-LENGTH* & *TRACE-MACROEXPAND-PRINT-LEVEL* are
not used.

The return value is ignored.")

(defvar *wrapped-macroexpand-hook*
  ;; the former value of *MACROEXPAND-HOOK*, used both to save &
  ;; restore, and also to call the wrapped hook.
  nil)

(defun trace-macroexpand-hook (macro-function macro-form environment)
  ;; Trace macros: this is installed as the value of *macroexpand-hook*
  (unless *wrapped-macroexpand-hook*
    (error "No wrapped *MACROEXPAND-HOOK*?"))
  (if (trace-macroexpand-trace-p macro-function macro-form environment)
      (let ((expanded-form (funcall *wrapped-macroexpand-hook*
                                    macro-function macro-form environment)))
        (if *trace-macroexpand-printer*
            (funcall *trace-macroexpand-printer* *trace-output*
                     macro-form expanded-form environment)
          (let ((*print-length* *trace-macroexpand-print-length*)
                (*print-level* *trace-macroexpand-print-level*)
                (*print-pretty* t))
            (format *trace-output* "~&~S~% -> ~S~%"
                    macro-form expanded-form)
            expanded-form)))
    (funcall *wrapped-macroexpand-hook*
             macro-function macro-form environment)))

(defun trace-macroexpand (&optional (tracep t))
  "Trace or untrace macroexpansion.

If called with no argument, or an argument which is true, ensure that
macroexpansion is on.  Otherwise ensure it is off.

Return the previous state."
  (let ((currently-tracing (if *wrapped-macroexpand-hook* t nil)))
    (cond ((and tracep (not currently-tracing))
           (setf *wrapped-macroexpand-hook* *macroexpand-hook*
                 *macroexpand-hook* #'trace-macroexpand-hook))
          ((and (not tracep) currently-tracing)
           (setf *macroexpand-hook* *wrapped-macroexpand-hook*
                 *wrapped-macroexpand-hook* nil)))
    currently-tracing))

(defun macroexpand-traced-p ()
  "Is macroexpansion currently traced?"
  (if *wrapped-macroexpand-hook* t nil))

(defun call/macroexpand-tracing (f &optional (state t))
  "Call f with macroexpansion tracing on (or off).

This is useful for compiling files, say, where you want to see what
happens."
  (let ((*macroexpand-hook* *macroexpand-hook*)
        (*wrapped-macroexpand-hook* *wrapped-macroexpand-hook*))
    (trace-macroexpand state)
    (funcall f)))

(defmacro with-macroexpand-tracing ((&optional (state 't))
                                    &body forms)
  "Evaluate FORMS with (or without) macroexpansion tracing

See CALL/MACROEXPAND-TRACING which this is a shim for."
  `(call/macroexpand-tracing (lambda () ,@forms) ,state))

;;;; Convenience macros & functions
;;;

(defun trace-macros (names)
  ;; Return the list of traced names after adding (reversing it makes
  ;; it make more sense in general)
  (unless (every #'symbolp names)
    (error "Not all of ~S are symbols" names))
  (dolist (name names (reverse *trace-macroexpand-traced-names*))
    (pushnew name *trace-macroexpand-traced-names*)))

(defun untrace-macros (names)
  ;; Return the list of traced names after removing (reversing it
  ;; makes it make more sense in general)
  (unless (every #'symbolp names)
    (error "Not all of ~S are symbols" names))
  (if (null names)
      (setf *trace-macroexpand-traced-names* '())
    (dolist (name names (reverse *trace-macroexpand-traced-names*))
      (setf *trace-macroexpand-traced-names*
            (delete name *trace-macroexpand-traced-names*)))))

(defmacro trace-macro (&rest macro-names)
  "Trace macros named by MACRO-NAMES, when macro tracing is on.

These macros don't need to be defined: they will be traced when they
are defined.  In fact they don't even need to be macros: if they're
not then nothing will happen but they won't ever be traced.

See *TRACE-MACROEXPAND-TRACED-NAMES* etc for the underlying mechanism.
TRACE-MACROEXPAND turns tracing on and off.

You probably want to set *TRACE-MACROEXPAND-TRACED-PACKAGES* to '() if
you use this, or you will get per-package tracing."
  `(trace-macros ',macro-names))

(defmacro untrace-macro (&rest macro-names)
  "Untrace macros named by MACRO-NAMES."
  `(untrace-macros ',macro-names))

(defun canonicalise-package-designator (designator)
  (cond
   ((or (eql designator 'nil)
        (eql designator 't))
    designator)
   ((or (symbolp designator)
        (stringp designator))
    (unless (find-package designator)
      (error "~S designates no package" designator))
    ;; return the string, not the package name, as this may
    ;; be a nickname
    (string designator))
   ((packagep designator)
    (package-name designator))
   (t
    (error "~S is not a valid package designator" designator))))

(defun trace-macro-package (&rest package-designators)
  "Trace macros in the packages in PACKAGE-DESIGNATORS

See *TRACE-MACROEXPAND-TRACED-PACKAGES* for details of what a package
designator means in this context.  Return the list of all package
designators.

Note this is a function, not a macro like TRACE-MACRO / TRACE"
  (setf *trace-macroexpand-traced-packages*
        (nunion
         (mapcar #'canonicalise-package-designator
                 *trace-macroexpand-traced-packages*)
         (mapcar #'canonicalise-package-designator
                 package-designators)
         :test #'equal)))

(defun untrace-macro-package (&rest package-designators)
  "Untrace macros in the packages in PACKAGE-DESIGNATORS

See *TRACE-MACROEXPAND-TRACED-PACKAGES* for details of what a package
designator means in this context.  Return the list of all package
designators.

Note this is a function, not a macro like UNTRACE-MACRO / UNTRACE"
  (setf *trace-macroexpand-traced-packages*
        (nset-difference
         (mapcar #'canonicalise-package-designator
                 *trace-macroexpand-traced-packages*)
         (mapcar #'canonicalise-package-designator
                 package-designators)
         :test #'equal)))

(eval-when (:load-toplevel :execute)
  ;; Turn macroexpansion tracing on when this is loaded
  (trace-macroexpand t))
