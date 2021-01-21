;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - wrapping-standard.lisp
;; Description       - Wrapping standard method combination
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Wed May 29 17:55:55 2002
;; Last Modified On  - Sun Aug 30 14:03:13 2020
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 4
;; Status            - Unknown
;; 
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Wrapping standard method combination
;;;
;;; wrapping-standard.lisp is copyright 2001,2012 by me, Tim Bradshaw, and
;;; may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.

(defpackage :org.tfeb.hax.wrapping-standard
  (:use :cl)
  (:export #:wrapping-standard))

(in-package :org.tfeb.hax.wrapping-standard)

(provide :org.tfeb.hax.wrapping-standard)

(define-method-combination wrapping-standard ()
  ;; Like standard but WRAPPING methods get called in
  ;; *most-specific-last* order, and before and after any other methods
  ;; The complete order is then:
  ;;
  ;; least specific wrapping method
  ;;  ... call-next-method ...
  ;;  most specific around method 
  ;;   ... call-next-method ...
  ;;   most specific before method ... least specific before method
  ;;    most specific primary method 
  ;;     [... call-next-method ... other primary methods ...]
  ;;   least specific after method ... most specific after method
  ;;  rest of most specific around method
  ;; rest of least specific wrapping method
  ;;
  ((around (:around))
   (wrapping (:wrapping) :order :most-specific-last)
   (before (:before))
   (primary () :required t)
   (after (:after)))
  (flet ((call-methods (methods)
           (mapcar #'(lambda (method)
                       `(call-method ,method))
                   methods)))
    (let* ((form (if (or before after (rest primary))
                     `(multiple-value-prog1
                          (progn ,@(call-methods before)
                            (call-method ,(first primary)
                                         ,(rest primary)))
                        ,@(call-methods (reverse after)))
                   `(call-method ,(first primary))))
           (around-form (if around
                            `(call-method ,(first around)
                                          (,@(rest around)
                                           (make-method ,form)))
                          form)))
      (if wrapping 
          `(call-method ,(first wrapping)
                        (,@(rest wrapping)
                         (make-method ,around-form)))
        around-form))))


#||
(defgeneric complicated (x &key cache recompute)
  (:method-combination wrapping-standard)
  (:method :wrapping (x &key (cache t) (recompute nil))
    (call-next-method x :cache cache :recompute recompute)))
||#
