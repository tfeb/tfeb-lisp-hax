;;;; Small utilities, used mostly by other haxks
;;;

;;; Try to make this work as a module
;;;
#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate)

(defpackage :org.tfeb.hax.utilities
  (:use :cl
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:export
   #:parse-docstring-body
   #:parse-simple-body))

(in-package :org.tfeb.hax.utilities)

(provide :org.tfeb.hax.utilities)

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

(defun parse-simple-body (decls/forms)
  "Parse a body which can not have a docstring

Return two values: a list of declarations and a list of forms"
  (with-collectors (decl body)
    (do* ((forms decls/forms (rest forms))
          (this (first forms) (first forms)))
         ((null forms))
      (if (and (consp this)
               (eql (first this) 'declare))
          (decl this)
        (body this)))))
