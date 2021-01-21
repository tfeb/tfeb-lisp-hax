;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - abstract-classes.lisp
;; Description       - Abstract classes in CL
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Sun Dec 10 18:21:40 2000
;; Last Modified On  - Sun Jan 17 16:49:17 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 17
;; Status            - Unknown
;; 
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Abstract classes
;;;
;;; abstract-classes.lisp is copyright 2000-2001, 2021 by me, Tim
;;; Bradshaw, and may be used for any purpose whatsoever by anyone. It
;;; has no warranty whatsoever. I would appreciate acknowledgement if
;;; you use it in anger, and I would also very much appreciate any
;;; feedback or bug fixes.
;;;

;;; Get Closer to MOP if this is being loaded as a module: this is
;;; needed at runtime as well as compile time so a feature expression
;;; won't work.
;;;
#-Lispworks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :closer-common-lisp)
    #+quicklisp
    (ql:quickload "closer-mop")
    #-quicklisp
    (error "abstract-classes needs Closer to MOP")))

(defpackage :org.tfeb.hax.abstract-classes
  (:nicknames :org.tfeb.hax.final-classes)
  #-LispWorks
  (:use :closer-common-lisp)
  #+LispWorks
  (:use :cl :hcl)
  (:export #:abstract-class
           #:define-abstract-class
           #:final-class
           #:define-final-class))

(in-package :org.tfeb.hax.abstract-classes)

(provide :org.tfeb.hax.abstract-classes)
(provide :org.tfeb.hax.final-classes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (eq 'standard-class 'cl:standard-class)
    (warn "STANDARD-CLASS isn't: you're probably doomed")))

(defclass abstract-class (standard-class)
  ()
  (:documentation "The class of abstract classes"))

(defmethod make-instance ((c abstract-class) &rest junk)
  (declare (ignore junk))
  (error "Trying to make an instance of ~A which is an abstract class"
         (class-name c)))

;;; The MOP requires this, but it's not clear that implementations do.
;;; VALIDATE-SUPERCLASS specifies when a superclass is suitable for a
;;; subclass. You have to be pretty specific, It's probably not in
;;; general safe to do what we do here.
;;;

(defmethod validate-superclass ((class abstract-class) 
                                (superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS...
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass abstract-class))
  ;; ... and the other way around.
  t)


;;; I don't want to have to say ... (:metaclass abstract-class), but
;;; there is no easy hook into processing the options to DEFCLASS:
;;; ENSURE-CLASS-USING-CLASS, which would be the logical place to do
;;; this, is called with a class of NIL if there is no existing class,
;;; and so can't usefully be specialized.
;;;

(defmacro define-abstract-class (class supers slots &rest options)
  (when (assoc ':metaclass options)
    (error "Defining an abstract class with a metaclass?"))
  `(defclass ,class ,supers ,slots
             ,@options
             (:metaclass abstract-class)))

;;; Samples of abstract classes
#||
(define-abstract-class abstract-thing ()
  ((s :accessor thing-s)))

(defclass thing (abstract-thing)
  ((s :initform 1)))
||#

;;; Benchmarks: for ACL 6.0 there is no performance hit.
#||
(define-abstract-class ac () ())
(defclass ac-instantiable (ac) ())
(defclass nac () ())
(defclass nac-instantiable (nac) ())

(defun make-n-aci (n)
  (declare (type fixnum n)
           (optimize speed))
  (loop repeat n
      do (make-instance 'ac-instantiable)))

(defun make-n-naci (n)
  (declare (type fixnum n)
           (optimize speed))
  (loop repeat n
      do (make-instance 'nac-instantiable)))

(defun make-n-general (n cn)
  (declare (type fixnum n)
           (optimize speed))
  (loop repeat n
      do (make-instance cn)))
||#


;;;; Final classes
;;;
;;; Classes which may not be subclassed.
;;;
;;; I just know someone is going to ask for an abstract final class.

(defclass final-class (standard-class)
  ()
  (:documentation "The class of classes which may not be subclassed"))

;;; The MOP requires this, but it's not clear that implementations do.
;;; VALIDATE-SUPERCLASS specifies when a superclass is suitable for a
;;; subclass. You have to be pretty specific, It's probably not in
;;; general safe to do what we do here.
;;;

(defmethod validate-superclass ((class final-class) 
                                (superclass standard-class))
  ;; This is, in general, somewhat too permissive, but we are going to
  ;; allow any instance of (a subclass of) STANDARD-CLASS to act as a
  ;; superclass of any instance of ABSTRACT-CLASS...
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass final-class))
  (error "Attempting to subclass a final class"))


;;; I don't want to have to say ... (:metaclass final-class), but
;;; there is no easy hook into processing the options to DEFCLASS:
;;; ENSURE-CLASS-USING-CLASS, which would be the logical place to do
;;; this, is called with a class of NIL if there is no existing class,
;;; and so can't usefully be specialized.
;;;

(defmacro define-final-class (class supers slots &rest options)
  (when (assoc ':metaclass options)
    (error "Defining a final class with a metaclass?"))
  `(defclass ,class ,supers ,slots
             ,@options
             (:metaclass final-class)))
