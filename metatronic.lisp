;;;; Metatronic macros
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.utilities :compile t))

(defpackage :org.tfeb.hax.metatronic
  (:use :cl :org.tfeb.hax.utilities)
  (:export
   #:defmacro/m
   #:macrolet/m
   #:metatronize))

(in-package :org.tfeb.hax.metatronic)

(provide :org.tfeb.hax.metatronic)

(defun metatronize (form &key
                         (rewrites '()) (shares '())
                         (rewriter nil))
  ;; This has hard-wired knowledge of what a metatronic variable looks
  ;; like unless REWRITER is given
  "Return a metatronic version of FORM, the table of variables, a list
of anonymous variables and the sharing table.

Arguments are FORM with keyword arguments
- REWRITES is a table of variables returned from a previous call
- SHARES is a sharing table returned from a previous call
- REWRITER, if given, should be a designator for a function of one
  argument, a symbol, which should either return the symbol or a
  metatronized symbol and an indication of whether it should be stored
  in the rewrite table.

This only looks at list structure.  Sharing and circularity of list
structure (only) is correctly copied."
  (let ((rtab rewrites) ;I feel bad assigning to arguments
        (stab shares)
        (anons '()))
    (labels ((rewrite (this)
               (typecase this
                 (symbol
                  (let ((r (assoc this rtab)))
                    (if r
                        (cdr r)
                      (if rewriter
                          (multiple-value-bind (new storep)
                              (funcall rewriter this)
                            (if (eq new this)
                                this
                              (progn
                                (if storep
                                    (setf rtab (acons this new rtab))
                                  (push new anons))
                                new)))
                        (let* ((n (symbol-name this))
                               (l (length n)))
                          (if (and (>= l 2)
                                   (char= (char n 0) #\<)
                                   (char= (char n (1- l)) #\>))
                              (let ((s (make-symbol n)))
                                (if (/= l 2)
                                    (setf rtab (acons this s rtab))
                                  (push s anons))
                                s)
                            this))))))
                 (cons
                  (let ((seen (assoc this stab)))
                    (if seen
                        (cdr seen)
                      (let ((new (cons nil nil)))
                        (setf stab (acons this new stab)
                              (car new) (rewrite (car this))
                              (cdr new) (rewrite (cdr this)))
                        new))))
                 (t
                  ;; Not going to handle arrays etc because it is a
                  ;; lot of work for almost or actually no benefit.
                  this))))
      (values (rewrite form) rtab anons stab))))

(defun m2 (form metatrons anonymous)
  ;; Second quantization: this just exists so macroexpansions are
  ;; smaller
  (values (metatronize form
                       :rewriter (lambda (s)
                                   (cond
                                    ((member s metatrons)
                                     (values (make-symbol (symbol-name s))
                                             t))
                                    ((member s anonymous)
                                     (values (make-symbol (symbol-name s))
                                             nil))
                                    (t
                                     (values s nil)))))))

(defmacro defmacro/m (name (&rest args) &body doc/decls/forms)
  "Define a metatronic macro

This is exactly like DEFMACRO but metatronic symbols are gensymized,
when they occur directly in list structure.

Note that metatronic symbols are *not* gensymized in arrays,
structures or what have you as it's just too hard.  Use
LOAD-TIME-VALUE to construct a literal at load time if you really need
this."
  (multiple-value-bind (doc decls forms) (parse-docstring-body doc/decls/forms)
    (multiple-value-bind (metatronized-forms rtab anons stab) (metatronize forms)
      (declare (ignore stab))
      `(defmacro ,name ,args
         ,@(if doc (list doc) '())
         ,@decls
         (m2 (progn ,@metatronized-forms)
                             ',(mapcar #'cdr rtab) ',anons)))))

(defmacro macrolet/m (clauses &body forms)
  "MACROLET, metatronically"
  `(macrolet
       ,(mapcar (lambda (clause)
                  (destructuring-bind (name (&rest args) &body doc/decls/forms) clause
                    (multiple-value-bind (doc decls forms) (parse-docstring-body doc/decls/forms)
                      (multiple-value-bind (metatronized-forms rtab anons stab)
                          (metatronize forms)
                        (declare (ignore stab))
                        `(,name ,args
                                ,@(if doc (list doc) '())
                                ,@decls
                                (m2 (progn ,@metatronized-forms)
                                                    ',(mapcar #'cdr rtab)
                                                    ',anons))))))
                clauses)
     ,@forms))

#+(and LispWorks LW-Editor)
(editor:setup-indent "macrolet/m" 1 nil nil 'flet)

#||
(defmacro/m do-file ((lv file) &body forms)
  `(let ((<file> ,file))
     (with-open-file (<in> <file>)
       (doing ((,lv (read-line <in> nil <in>)))
              ((eq ,lv <in>) <file>)
         ,@forms))))
||#
