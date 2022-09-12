;;;; Hairy function & macro definition macros
;;;
;;; define-functions.lisp is copyright 2020, 2022 by me, Tim Bradshaw,
;;; and may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.
;;;

(defpackage :org.tfeb.hax.define-functions
  (:use :cl)
  (:export
   #:define-function
   #:define-functions
   #:define-macro-function
   #:define-macro-functions))

(in-package :org.tfeb.hax.define-functions)

(provide :org.tfeb.hax.define-functions)

(defmacro define-functions (names/options &body fns/form)
  "Hairy function-defining form.

NAMES/OPTIONS is a list of specifications for functions.  Each element
may be either:
- a function name such as FOO or (SETF FOO);
- a lambda list (function-name &key ftype documentation);

In the second case:
- ftype specifies the function type, which is by default FUNCTION;
- documentation is function documentation (none by default).

If the body of the definition is a single form, this is assumed to
return as many values as there are functions to define.  Otherwise
there should be as many forms in the body as there are functions to
define, and the value of each form is used for the corresponding
function."
  (multiple-value-bind (names ftypes docs)
      ;; The parsing is just kind of horrid
      (loop for n/o in names/options
            for (name ftype doc)
              = (cond
                 ((or (symbolp n/o)
                      (and (consp n/o)
                           (eq (first n/o) 'setf)))
                  (list n/o 'function nil))
                 ((consp n/o)
                  (destructuring-bind (name &key
                                            (ftype 'function)
                                            (documentation nil))
                      n/o
                    (list name ftype documentation)))
                 (t
                  (error "bad name/options ~A" n/o)))
            collect name into the-names
            collect ftype into the-ftypes
            collect doc into the-docs
            finally (return (values the-names the-ftypes the-docs)))
    ;; Make sure function definitions are
    (assert (every (lambda (name)
                     (or (symbolp name)
                         (and (consp name)
                              (eq (first name) 'setf)
                              (symbolp (second name))
                              (= (list-length name) 2))))
                   names)
        (names)
      "bad function names in ~A" names)
    `(progn
       ;; Type declaraions must happen at top level to teach the
       ;; compiler about the functions, and documentation might as
       ;; well.
       (declaim ,@(loop for name in names
                        for ftype in ftypes
                        collect `(ftype ,ftype ,name)))
       ,@(loop for name in names
               for doc in docs
               when doc
                 collect `(setf (documentation ',name 'function)
                                ,doc))
       ;; But the actual definition of them does not need to happen
       ;; until runtime
       ,@(if (= (length fns/form) 1)
             ;; values case
             (let ((tmpns (loop for i from 0 below (length names)
                                collect (make-symbol (format nil "F~D" i)))))
               `((multiple-value-bind ,tmpns ,(first fns/form)
                   ,@(loop for n in names
                           for tmpn in tmpns
                           collect `(unless (functionp ,tmpn)
                                      (error "function for ~S isn't" ',n))
                           collect `(setf (fdefinition ',n) ,tmpn)))))
           ;; listy case
           (progn
             (unless (= (length fns/form) (length names))
               (error "expected ~D form~:P, got ~D"
                      (length names) (length fns/form) ))
             (loop with tmpn = (make-symbol "F")
                   for name in names
                   for form in fns/form
                   collect `(let ((,tmpn ,form))
                              (unless (functionp ,tmpn)
                                (error "function for ~S isn't" ',name))
                              (setf (fdefinition ',name) ,tmpn)))))
       (values ,@(loop for n in names collect `',n)))))

(defmacro define-function (name/options fn)
  "Define a single function.

NAME/OPTIONS is interpreted as DEFINE-FUNCTIONS, which see."
  `(define-functions (,name/options) ,fn))

#+LispWorks (editor:setup-indent "define-function" 1)

;;; Examples of define-functions / define-functions
;;;

#||
(define-functions (up down)
  (let ((c 0))
    (values
     (lambda ()
       (incf c))
     (lambda ()
       (decf c)))))

(define-function (inc
                  :ftype (function (number) number)
                  :documentation "increment a number")
  (lambda (n)
    (1+ n))) ||#

(defmacro define-macro-functions (names/options &body fns/form)
  "Hairy macro-function defining form.

NAMES/OPTIONS is a list of specifications for macro functions.  Each
element may be either: - a macro name such as FOO or (SETF FOO); - a
lambda list (macro-name &key documentation);

In the second case documentation is macro documentation (none by
default).

If the body of the definition is a single form, this is assumed to
return as many values as there are macros to define.  Otherwise there
should be as many forms in the body as there are functions to define,
and the value of each form is used for the corresponding function.

Note that macro functions all take exactly two arguments."
  ;; This is much less useful than DEFINE-FUNCTION / DEFINE-FUNCTIONS
  ;; but I feel it should exist.  It is at least simpler.
  (multiple-value-bind (names docs)
      ;; The parsing is just kind of horrid
      (loop for n/o in names/options
            for (name doc)
              = (cond
                 ((symbolp n/o)
                  (list n/o nil))
                 ((consp n/o)
                  (destructuring-bind (name &key
                                            (documentation nil))
                      n/o
                    (list name documentation)))
                 (t
                  (error "bad name/options ~A" n/o)))
            collect name into the-names
            collect doc into the-docs
            finally (return (values the-names the-docs)))
    (assert (every #'symbolp names)
        (names)
      "bad macro names in ~A" names)
    `(eval-when (:load-toplevel :compile-toplevel :execute)
       ;; Macros are needed at compile time as well
       ,@(if (= (length fns/form) 1)
             ;; values case
             (let ((tmpns (loop for i from 0 below (length names)
                                collect (make-symbol (format nil "F~D" i)))))
               `((multiple-value-bind ,tmpns ,(first fns/form)
                   ,@(loop for n in names
                           for tmpn in tmpns
                           for d in docs
                           collect `(unless (functionp ,tmpn)
                                      (error "macro function for ~S isn't" ',n))
                           collect `(setf (macro-function ',n) ,tmpn)
                           when d
                             collect `(setf (documentation ',n 'function)
                                            ,d)))))
           ;; listy case
           (progn
             (unless (= (length fns/form) (length names))
               (error "expected ~D form~:P, got ~D"
                      (length names) (length fns/form)))
             (loop with tmpn = (make-symbol "F")
                   for name in names
                   for d in docs
                   for form in fns/form
                   collect `(let ((,tmpn ,form))
                              (unless (functionp ,tmpn)
                                (error "macro function for ~S isn't" ',name))
                              (setf (macro-function ',name) ,tmpn))
                   when d
                     collect
                       `(setf (documentiation ',name 'function) d))))
       (values ,@(loop for n in names collect `',n)))))

(defmacro define-macro-function (name/options fn)
  "Define a single macro function.

NAME/OPTIONS is interpreted as DEFINE-MACRO-FUNCTIONS, which see."
  `(define-macro-functions  (,name/options) ,fn))

#+LispWorks
(editor:setup-indent "define-macro-function" 1)

;;; A silly example of define-macro-functions
;;;

#||
(define-macro-functions ((funging :documentation "incompatible with frobbing")
                         (frobbing :documentation "incompatible with funging"))
  (let ((which nil))
    (values
     (lambda (form environment)
       (declare (ignore environment))
       (unless (or (not which) (eq which 'funging))
         (error "can't funge and frob"))
       (setf which 'funging)
       `(progn ,@(rest form)))
     (lambda (form environment)
       (declare (ignore environment))
       (unless (or (not which) (eq which 'frobbing))
         (error "can't funge and frob"))
       (setf which 'frobbing)
       `(progn ,@(rest form))))))
||#

#||
;;; I am sure this serves no useful purpose: I thought it did but I
;;; was wrong
;;;

(defmacro define-variables (names/options &body vs/form)
  "Hairy variable-defining form.

NAMES/OPTIONS is a list of specifications for variables.  Each element
may be either:
- a variable name;
- a lambda list (variable-name &key type documentation).

In the second case:
- type specifies the variable's type;
- documentation is documentation (none by default).

If the body of the definition is a single form, this is assumed to
return as many values as there are variables to define.  Otherwise
there should be as many forms in the body as there are functions to
define, and the value of each form is used for the corresponding
variable.

This is like DEFPARAMETER not DEFVAR."
  (multiple-value-bind (names types docs)
      ;; The parsing is just kind of horrid
      (loop for n/o in names/options
            for (name type doc)
              = (typecase n/o
                  (symbol
                   (list n/o t nil))
                  (cons
                   (destructuring-bind (name &key
                                             (type 't)
                                             (documentation nil))
                       n/o
                     (list name type documentation)))
                  (t
                   (error "bad name/options ~A" n/o)))
            collect name into the-names
            collect type into the-types
            collect doc into the-docs
            finally (return (values the-names the-types the-docs)))
    (assert (every #'symbolp names)
        (names)
      "bad variable names in ~A" names)
    `(progn
       ;; These must happen at top level to teach the compiler
       (declaim
        ,@(loop for name in names
                for type in types
                unless (eq type t)      ;no point if type is T
                  collect `(type ,type ,name)))
       ,@(loop for name in names
                 collect `(defvar ,name))
       ,@(loop for name in names
               for doc in docs
               when doc
                 collect `(setf (documentation ',name 'variable) ',doc))
       ,@(if (= (length vs/form) 1)
            ;; values case
            (let ((tmpns (loop for i from 0 below (length names)
                               collect (make-symbol (format nil "V~D" i)))))
              `((multiple-value-bind ,tmpns ,(first vs/form)
                 ,@(loop for n in names
                         for tmpn in tmpns
                         collect `(setf ,n ,tmpn)))))
          ;; listy case
          (progn
            (unless (= (length vs/form) (length names))
              (error "expected ~D form~:P, got ~D"
                     (length names) (length vs/form)))
            (loop for name in names
                  for form in vs/form
                  collect `(setf ,name ,form))))
       (values ,@(loop for n in names collect `',n)))))
||#
