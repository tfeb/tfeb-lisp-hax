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
;;; - (bind/values (...) form ...) binds multiple values -- if there
;;;   is a single form it should return as many values as there are
;;;   variables, otherwise there should be as many forms as values;
;;; - (bind/destructuring dsll form) binds with destructuring;
;;;
;;; Successive bindings of the same kind (for BIND, not the other two)
;;; are coalesced.
;;;

;;; Try to make this work as a module
;;;
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (and (member "ORG.TFEB.HAX.COLLECTING" *modules* :test #'string=)
               (member "ORG.TFEB.HAX.ITERATE" *modules* :test #'string=))
    #+org.tfeb.tools.require-module
    (org.tfeb.tools.require-module:require-modules
     '(:org.tfeb.hax.collecting
       :org.tfeb.hax.iterate))
    #-org.tfeb.tools.require-module
    (error "doomed")))

(defpackage :org.tfeb.hax.binding
  (:use :cl
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:export
   #:binding
   #:bind
   #:bind/values
   #:bind/destructuring))

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

(defun parse-binding-form (form)
  ;; Return what sort of binding form this is, or NIL, and the
  ;; corresponding binding, or NIL
  (if (consp form)
      (case (first form)
        ((bind)
         (unless (>= (length form) 2)
           (error "hopless bind form ~S" form))
         (typecase (first (rest form))
             (symbol
              (unless (<= (length form) 3)
                (error "too many expressions in ~S" form))
              (values 'variable (rest form)))
             (cons
              ;; This is a local function: some slight parsing is
              ;; needed here
              (destructuring-bind ((name &rest args) &body decls/body)
                  (rest form)
                (iterate parse-decls ((body decls/body)
                                      (slced '()))
                  (if (and (consp body)
                           (consp (first body))
                           (eq (car (first body)) 'declare))
                      (parse-decls (rest body) (cons (first body)
                                                     slced))
                    (values 'function `(,name ,args
                                              ,@(reverse slced)
                                              (binding ,@body)))))))
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
        (otherwise
         (values nil nil)))
    (values nil nil)))

(defun walk-binding-body (body)
  ;; Walk the body of a BINDING form.  This is just unavoidably hairy.
  (collecting
    (iterate wbb ((tail body)
                  (variable-bindings '())
                  (function-bindings '()))
      (cond
       (tail
        (destructuring-bind (this . rest) tail
          (multiple-value-bind (what binding) (parse-binding-form this)
            (ecase what
              ((variable)
               (cond
                (variable-bindings      ;been collecting vars
                 (wbb rest (cons binding variable-bindings)
                      function-bindings))
                (function-bindings      ;been collecting fns
                 (collect `(labels ,(reverse function-bindings)
                             ,@(walk-binding-body tail))))
                (t                      ;not collecting
                 (wbb rest (cons binding variable-bindings) '()))))
              ((function)
               (cond
                (function-bindings
                 (wbb rest '() (cons binding function-bindings)))
                (variable-bindings
                 (collect `(let* ,(reverse variable-bindings)
                             ,@(walk-binding-body tail))))
                (t
                 (wbb rest '() (cons binding function-bindings)))))
              ((values)
               (cond
                (variable-bindings
                 (collect
                  `(let* ,(reverse variable-bindings)
                     (multiple-value-bind ,(first binding) ,(second binding)
                       ,@(walk-binding-body rest)))))
                (function-bindings
                 (collect
                  `(labels ,(reverse function-bindings)
                     (multiple-value-bind ,(first binding) ,(second binding)
                       ,@(walk-binding-body rest)))))
                (t
                 (collect
                  `(multiple-value-bind ,(first binding) ,(second binding)
                     ,@(walk-binding-body rest))))))
              ((destructuring)
               (cond
                (variable-bindings
                 (collect
                  `(let* ,(reverse variable-bindings)
                     (destructuring-bind ,(first binding) ,(second binding)
                       ,@(walk-binding-body rest)))))
                (function-bindings
                 (collect
                  `(labels ,(reverse function-bindings)
                     (destructuring-bind ,(first binding) ,(second binding)
                       ,@(walk-binding-body rest)))))
                (t
                 (collect
                  `(destructuring-bind ,(first binding) ,(second binding)
                     ,@(walk-binding-body rest))))))
              ((nil)
               (cond
                (variable-bindings
                 (collect
                  `(let* ,(reverse variable-bindings)
                     ,this
                     ,@(walk-binding-body rest))))
                (function-bindings
                 (collect
                  `(labels ,(reverse function-bindings)
                     ,this
                     ,@(walk-binding-body rest))))
                (t
                 (collect this)
                 (wbb rest '() '()))))))))
       (variable-bindings
        ;; hit end of body with pending variables: this only matters
        ;; for side-effect
        (collect `(let* ,(reverse variable-bindings))))
       (function-bindings
        ;; Pending functions, matters even less
        (collect `(labels ,(reverse function-bindings))))))))

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
             (values a b)))))
  (destructuring-bind (form expansion) form/expansion
    (unless (equal (macroexpand-1 form) expansion)
      (warn "~S expanded to ~S, not ~S"
            form (macroexpand-1 form) expansion))))
