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

(defpackage :org.tfeb.hax.object-accessors
  (:use :cl)
  (:export #:with-object-accessors))

(in-package :org.tfeb.hax.object-accessors)

(provide :org.tfeb.hax.object-accessors)

(defmacro with-object-accessors ((&rest accessor-specs) object &body decls/forms)
  "This is exactly like WITH-ACCESSORS but for completely general objects.

For instance:
    (with-oject-accessors ((kar car) cdr) (cons 1 2)
      ... kar ... cdr ...)
"
  (let ((itn (make-symbol "IT")))
    `(let ((,itn ,object))
       (symbol-macrolet
           ,(mapcar (lambda (s)
                      (typecase s
                        (symbol
                         `(,s (,s ,itn)))
                        (cons
                         (unless (and (= (list-length s) 2)
                                      (symbolp (first s)))
                           (error "bad accessor spec ~A" s))
                         `(,(first s) (,(second s) ,itn)))
                        (t
                         (error "bad accessor spec ~A" s))))
                    accessor-specs)
         ,@decls/forms))))
