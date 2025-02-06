;;;; A macro like with-accessors for general objects
;;;
;;; This is as trivial as it looks.
;;;
;;; object-accessors.lisp is copyright 2021 by me, Tim Bradshaw, and
;;; may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.utilities :compile t))

(defpackage :org.tfeb.hax.object-accessors
  (:use :cl)
  (:use :org.tfeb.hax.utilities)
  (:export
   #:with-object-accessors
   #:with-named-array-references))

(in-package :org.tfeb.hax.object-accessors)

(provide :org.tfeb.hax.object-accessors)

(defmacro with-object-accessors ((&rest accessor-specs) object &body decls/forms)
  "This is exactly like WITH-ACCESSORS but for completely general objects.

For instance:
    (with-oject-accessors ((kar car) cdr) (cons 1 2)
      ... kar ... cdr ...)
"
  (with-names (<it>)
    `(let ((,<it> ,object))
       (symbol-macrolet
           ,(mapcar (lambda (s)
                      (typecase s
                        (symbol
                         `(,s (,s ,<it>)))
                        (cons
                         (unless (and (= (list-length s) 2)
                                      (symbolp (first s)))
                           (error "bad accessor spec ~A" s))
                         `(,(first s) (,(second s) ,<it>)))
                        (t
                         (error "bad accessor spec ~A" s))))
                    accessor-specs)
         ,@decls/forms))))

(defmacro with-named-array-references ((array &optional (type 'array)) (&rest refs)
                                       &body decls/forms)
  ;; Arguments are in the other order than WITH-OBJECT-ACCESSORS: I'm
  ;; uncomfortable about that.
  "Provide named accessors for elements of an array"
  (with-names (<a>)
    `(let ((,<a> ,array))
       (declare (type ,type ,<a>))
       (symbol-macrolet ,(mapcar (lambda (ref)
                                   `(,(first ref) (aref ,<a> ,@(rest ref))))
                                 refs)
         ,@decls/forms))))
