;;;; Local definitions (like Racket)
;;;
;;; Like Racket (rather than Scheme) bindings can occur anywhere in a
;;; BINDING form and an appropriately nested structure results.
;;;
;;; Bindings are only considered in the immediate children of BINDING
;;; to avoid needing a code-walker.
;;;
;;; Within BINDING
;;; - (bind var val) binds a variable;
;;; - (bind (f ...) ...) binds a function (punning syntax like Scheme);
;;; - (bind/macro m e) turns into (symbol-macrolet ((m e)) ...)
;;; - (bind/macro (m ...) ...) turns into (macrolet ((m (...) ...)) ...);
;;; - (bind/values (...) form ...) binds multiple values -- if there
;;;   is a single form it should return as many values as there are
;;;   variables, otherwise there should be as many forms as values;
;;; - (bind/destructuring dsll form) binds with destructuring;
;;;
;;; Successive bindings of the same kind (for BIND, not the other two)
;;; are coalesced.
;;;
;;; binding.lisp is copyright 2021 by me, Tim Bradshaw, and may be
;;; used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.
;;;

;;; Try to make this work as a module
;;;
#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate)

(defpackage :org.tfeb.hax.binding
  (:use :cl
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:export
   #:binding
   #:bind
   #:bind/values
   #:bind/destructuring
   #:bind/macro))

(in-package :org.tfeb.hax.binding)

(provide :org.tfeb.hax.binding)

;;; At top-level all these should be errors
;;;

(defmacro bind (name &body forms)
  (declare (ignore forms))
  (error "Trying to bind ~S outside binding" name))

(defmacro bind/values (vars &body forms)
  (declare (ignore forms))
  (error "Trying to bind/values ~S outside binding" vars))

(defmacro bind/destructuring (dsll form)
  (declare (ignore form))
  (error "Tryng to bind/destructuring ~A outside binding" dsll))

(defmacro bind/macro (name &body forms)
  (declare (ignore forms))
  (error "Trying to bind a macro ~S outside binding" name))

(defun parse-binding-form (form)
  ;; Return what sort of binding form this is, or NIL, and the
  ;; corresponding binding, or NIL
  (flet ((parse-fdef (what fdef)
           (destructuring-bind ((name &rest args) &body decls/body) fdef
             (iterate d-b ((body decls/body)
                           (slced '()))
               (if (and (consp body)
                        (consp (first body))
                        (eq (car (first body)) 'declare))
                   (d-b (rest body) (cons (first body) slced))
                 (values what
                         `(,name ,args
                                 ,@(reverse slced)
                                 (binding ,@body))))))))
    (if (consp form)
        (case (first form)
          ((bind)
           (unless (>= (length form) 2)
             (error "hopeless bind form ~S" form))
           (typecase (first (rest form))
             (symbol                    ;variable
              (unless (<= (length form) 3)
                (error "too many expressions in ~S" form))
              (values 'variable (rest form)))
             (cons                      ;local function
              (parse-fdef 'function (rest form)))
             (t
              (error "mutant bind form ~S" form))))
          ((bind/values)
           (unless (>= (length form) 2)
             (error "hopeless bind/values form ~S" form))
           (let ((vars (first (rest form)))
                 (forms (rest (rest form))))
             (unless (and (listp vars)
                          (every #'symbolp vars))
               (error "not all variables are in ~S" form))
             (values 'values
                     (if (= (length form) 3)
                         (rest form)
                       `(,vars (values ,@forms))))))
          ((bind/destructuring)
           (unless (= (length form) 3)
             (error "hopeless bind/destructing form ~S" form))
           (let ((dsll (first (rest form))))
             (unless (listp dsll)
               (error "destructuring lambda list isn't in ~S" form))
             (values 'destructuring (rest form))))
          ((bind/macro)
           (typecase (first (rest form))
             (symbol                      ;symbol macro
              (unless (= (length form) 3)
                (error "too many expressions for symbol macro in ~S" form))
              (values 'symbol-macro (rest form)))
             (cons                        ;macrolet
              (parse-fdef 'macro (rest form)))
             (t
              (error "mutant bind/macro form ~S" form))))
          (otherwise
           (values nil nil)))
      (values nil nil))))

(defun walk-binding-body (body)
  ;; Walk the body of a BINDING form.  This is just unavoidably hairy
  ;; (but less hairy than it was)
  (collecting
    (flet ((spit/punt (what binding/s more)
             ;; spit some bindings and punt
             (collect
              (ecase what
                ((variable)
                 `(let* ,(reverse binding/s)
                    ,@(walk-binding-body more)))
                ((values)
                 `(multiple-value-bind ,(first binding/s) ,(second binding/s)
                    ,@(walk-binding-body more)))
                ((destructuring)
                 `(destructuring-bind ,(first binding/s) ,(second binding/s)
                    ,@(walk-binding-body more)))
                ((function)
                 `(labels ,(reverse binding/s)
                    ,@(walk-binding-body more)))
                ((macro)
                 `(macrolet ,(reverse binding/s)
                    ,@(walk-binding-body more)))
                ((symbol-macro)
                 `(symbol-macrolet ,(reverse binding/s)
                    ,@(walk-binding-body more)))))))
    (iterate wbb ((tail body)
                  (current nil)
                  (bindings '()))
      (cond
       (tail                            ;more to do
        (destructuring-bind (this . more) tail
          (multiple-value-bind (what binding) (parse-binding-form this)
            (cond
             ;; Many tests here are done redundantly: the aim being to
             ;; catch bugs in my assumptions rather than silently
             ;; blunder on.
             ((and current (not (eql what current)))
              ;; were collecting something and whatever we found is
              ;; not it: spit current bindings and restart on tail
              (spit/punt current bindings tail))
             ((and (member what '(values destructuring)) (not current))
              ;; these are not composable: spit and punt on more
              (spit/punt what binding more))
             ((and (not what) (not current))
              ;; Just a form, collect it
              (collect this)
              (wbb more nil '()))
             ((eql what current)
              ;; something we're already accumulating
              (wbb more current (cons binding bindings)))
             ((and (not current) what)
              ;; A new thing to collect
              (wbb more what (list binding)))
             (t
              ;; can't happen
              (error "fell off the end: what is ~S, current is ~S"
                     what current))))))
       (current                        ;end, but were collecting
        (spit/punt current bindings tail))
       (t                              ;end, all done
        '()))))))

(defmacro binding (&body forms)
  ;; The macro itself
  (let ((expanded (walk-binding-body forms)))
    (if (= (length expanded) 1)
        (first expanded)
      `(progn ,@expanded))))

;;; Rudimentary sanity tests
;;;
(dolist (form/expansion
         '(((binding
             (bind a 1)
             (bind b 2)
             (values a b))
            (let* ((a 1) (b 2))
             (values a b)))
           ((binding
             1
             (bind b 2)
             b)
            (progn
              1
              (let* ((b 2))
                b)))
           ((binding
             (bind (f &rest args) args)
             (f 1 3))
           (labels ((f (&rest args)
                      (binding args)))
             (f 1 3)))
           ((binding
              (bind (f x)
                (declare (type fixnum x))
                x)
              (f 1))
            (labels ((f (x)
                       (declare (type fixnum x))
                       (binding x)))
              (f 1)))
           ((binding
             (bind/values (a b) (values 1 2))
             (values a b))
            (multiple-value-bind (a b) (values 1 2)
             (values a b)))
           ((binding
              (bind/values (a b) 1 2)
              (values a b))
            (multiple-value-bind (a b) (values 1 2)
             (values a b)))
           ((binding
             (bind/destructuring (a &rest b) (list 1 2))
             (values a b))
            (destructuring-bind (a &rest b) (list 1 2)
             (values a b)))
           ((binding
              (bind/macro (x y) y)
              z)
            (macrolet ((x (y) (binding y)))
              z))
           ((binding
              (bind/macro a 2)
              a)
            (symbol-macrolet ((a 2))
              a))))
  (destructuring-bind (form expansion) form/expansion
    (unless (equal (macroexpand-1 form) expansion)
      (warn "~S expanded to~% ~S,~%not~% ~S"
            form (macroexpand-1 form) expansion))))
