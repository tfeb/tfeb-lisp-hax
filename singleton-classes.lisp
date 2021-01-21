;;;;;;;;;;;;;;;;;;;;;;;;;;;;;; -*- Mode: Lisp -*- ;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; File              - singleton-classes.lisp
;; Description       - Singleton classes
;; Author            - Tim Bradshaw (tfb at lostwithiel)
;; Created On        - Tue Apr 30 14:22:26 2002
;; Last Modified On  - Tue Jan 19 15:29:14 2021
;; Last Modified By  - Tim Bradshaw (tfb at kingston.fritz.box)
;; Update Count      - 6
;; Status            - Unknown
;;
;; $Id$
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;;;; Singleton classes using the MOP
;;;
;;; singleton-classes.lisp is copyright 2002, 2021 by me, Tim Bradshaw,
;;; and may be used for any purpose whatsoever by anyone. It has no
;;; warranty whatsoever. I would appreciate acknowledgement if you use
;;; it in anger, and I would also very much appreciate any feedback or
;;; bug fixes.
;;;

;;; Get Closer to MOP if this is being loaded as a module: this is
;;; needed at runtime as well as compile time so a feature expression
;;; won't work.
;;;
#-LispWorks
(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :closer-common-lisp)
    #+quicklisp
    (ql:quickload "closer-mop")
    #-quicklisp
    (error "singleton-classes needs Closer to MOP")))

(defpackage :org.tfeb.hax.singleton-classes
  #-LispWorks
  (:use :closer-common-lisp)
  #+LispWorks
  (:use :cl :hcl)
  (:export #:singleton-class
           #:reset-singleton-classes))

(in-package :org.tfeb.hax.singleton-classes)

(provide :org.tfeb.hax.singleton-classes)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (eq 'standard-class 'cl:standard-class)
    (warn "STANDARD-CLASS isn't: you're probably doomed")))

(defclass singleton-class (standard-class)
  ((instance :initform nil)))

(defmethod validate-superclass ((class singleton-class)
                                (superclass standard-class))
  ;; it's OK for a standard class to be a superclass of a singleton
  ;; class
  t)

(defmethod validate-superclass ((class singleton-class)
                                (superclass singleton-class))
  ;; it's OK for a singleton class to be a subclass of a singleton class
  t)

(defmethod validate-superclass ((class standard-class)
                                (superclass singleton-class))
  ;; but it is not OK for a standard class which is not a singleton class
  ;; to be a subclass of a singleton class
  nil)

(defmethod make-instance ((class singleton-class)
                          &key)
  (with-slots (instance) class
    (or instance
        (setf instance (call-next-method)))))

(defvar *singleton-classes* '())

(defmethod initialize-instance :after ((c singleton-class) &key)
  (pushnew c *singleton-classes*))

(defun reset-singleton-classes ()
  ;; This means you will get new singletons from now on.
  (loop for c in *singleton-classes*
        do (setf (slot-value c 'instance) nil)))

#||
(defclass foo ()
  ()
  (:metaclass singleton-class))
||#
