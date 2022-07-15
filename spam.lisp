;;;; SPAM: the Simple Pattern Matcher
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.simple-loops :compile t))

(defpackage :org.tfeb.hax.spam
  (:use :cl :org.tfeb.hax.simple-loops)
  (:export
   #:head-matches
   #:list-matches
   #:list*-matches
   #:cons-matches
   #:list-of
   #:repeating-list-of
   #:is
   #:is-type
   #:any
   #:var
   #:lambda-list-keyword
   #:some-of
   #:all-of
   #:none-of
   #:matchp
   #:matching))

(in-package :org.tfeb.hax.spam)

(provide :org.tfeb.hax.spam)

;;; Predicate constructors
;;;
;;; This is about a third of a pattern matching thing all on its own
;;;

(defun head-matches (&rest predicates)
  ;; Return a predicate which matches a lambda list if the leading
  ;; section of it matches PREDICATES.  A compiler macro for this
  ;; would be fun to write.
  (declare (dynamic-extent predicates))
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
    (lambda (ll)
      (looping ((lt ll) (pt effective-predicates))
        (cond
         ((null pt) (return t))
         ((not (consp lt)) (return nil))
         ((funcall (first pt) (first lt))
          (values (rest lt) (rest pt)))
         (t (return nil)))))))

(defun list-matches (&rest predicates)
  ;; Return a predicate which matches the whole of a list, which must
  ;; be a list.
  (declare (dynamic-extent predicates))
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
    (lambda (ll)
      (looping ((lt ll) (pt effective-predicates))
        (cond
         ((null pt) (return (null lt)))
         ((not (consp lt)) (return nil))
         ((funcall (first pt) (first lt))
          (values (rest lt) (rest pt)))
         (t (return nil)))))))

(defun list*-matches (&rest predicates)
  (declare (dynamic-extent predicates))
  (when (null predicates)
    (error "need at least one predicate for the tail"))
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
    (lambda (ll)
      (looping ((lt ll) (pt effective-predicates))
        (cond
         ((null (rest pt)) (return (funcall (first pt) lt)))
         ((not (consp lt)) (return nil))
         ((funcall (first pt) (first lt))
          (values (rest lt) (rest pt)))
         (t (return nil)))))))

(defun cons-matches (car cdr)
  ;; Return a predicate which matches a cons where the car and cdr
  ;; match CAR & CDR.
  (lambda (c)
    (and (consp c)
         (funcall car (car c))
         (funcall cdr (cdr c)))))

(defun list-of (predicate)
  (let ((effective-predicate (etypecase predicate
                                (function predicate)
                                (symbol (symbol-function predicate)))))
    (lambda (ll)
      (looping ((llt ll))
        (cond
         ((null llt) (return t))
         ((not (consp llt)) (return nil))
         ((funcall effective-predicate (first llt))
          (rest llt))
         (t (return nil)))))))

(defun repeating-list-of (&rest predicates)
  ;; (repeating-list-of #'keywordp (any)), for instance
  (declare (dynamic-extent predicates))
  (when (null predicates)
    (error "need at least one predicate to repeat"))
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
    (lambda (ll)
      (looping ((llt ll)
                (ept '()))
        (if (null ept)
            (if (null llt)
                (return t)
              (values llt effective-predicates))
          (cond
           ((not (consp llt)) (return nil))
           ((funcall (first ept) (first llt))
            (values (rest llt) (rest ept)))
           (t (return nil))))))))

(defun is (x)
  ;; return a predicate which matches EQL
  (lambda (v)
    (eql v x)))

(defun is-type (type)
  (lambda (v)
    (typep v type)))

(defun any ()
  ;; return a predicate which always succeeds
  (lambda (v)
    (declare (ignore v))
    t))

(defun var ()
  (lambda (v)
    (and (symbolp v)
         (not (constantp v))            ;includes NIL
         (not (or (keywordp v)
                  (member v lambda-list-keywords))))))

(defun lambda-list-keyword ()
  (lambda (v)
    (member v lambda-list-keywords)))

(defun some-of (&rest predicates)
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
  (lambda (e)
    (some (lambda (p)
            (funcall p e)) effective-predicates))))

(defun all-of (&rest predicates)
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
  (lambda (e)
    (every (lambda (p)
             (funcall p e))
           effective-predicates))))

(defun none-of (&rest predicates)
  (let ((effective-predicates (mapcar (lambda (p)
                                        (etypecase p
                                          (function p)
                                          (symbol (symbol-function p))))
                                      predicates)))
  (lambda (e)
    (notany (lambda (p)
              (funcall p e))
            effective-predicates))))

(defun matchp (thing predicate)
  (funcall predicate thing))

(defmacro matching (form &body clauses)
  (let ((<var> (make-symbol "V")))
    `(let ((,<var> ,form))
       (cond
        ,@(mapcar (lambda (clause)
                    (destructuring-bind (matcher &body forms) clause
                      (case matcher
                        ((otherwise t)
                         `(t ,@forms))
                        (otherwise
                         `((funcall ,matcher ,<var>)
                           ,@forms)))))
                  clauses)))))
