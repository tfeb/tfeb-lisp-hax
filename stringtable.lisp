;;;; Stringtables, or special strings
;;;
;;; The real goal of this is to be able to write strings which can
;;; ignore newlines.
;;;
;;; stringtable.lisp is copyright 2021 by me, Tim Bradshaw, and may be
;;; used for any purpose whatsoever by anyone. It has no warranty
;;; whatsoever. I would appreciate acknowledgement if you use it in
;;; anger, and I would also very much appreciate any feedback or bug
;;; fixes.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 :org.tfeb.hax.iterate)

(defpackage :org.tfeb.hax.stringtable
  (:use :cl :org.tfeb.tools.require-module
   :org.tfeb.hax.collecting
   :org.tfeb.hax.iterate)
  (:export
   #:*stringtable*
   #:*stringtable-fallback-absorb-special*
   #:copy-stringtable
   #:stringtable-escape-character
   #:make-stringtable-special-character
   #:get-stringtable-special-character
   #:set-stringtable-special-character
   #:read-string-with-stringtable
   #:make-stringtable-readtable
   #:set-stringtable-newline-skipper))

(in-package :org.tfeb.hax.stringtable)
(provide :org.tfeb.hax.stringtable)

(define-condition stringtable-error (simple-error)
  ((message :initform "Stringtable error"
            :reader stringtable-error-message))
  (:report
   (lambda (condition stream)
     (format stream "~A: ~?"
             (stringtable-error-message condition)
             (simple-condition-format-control condition)
             (simple-condition-format-arguments condition)))))

(define-condition stringtable-reader-error (stringtable-error reader-error)
  ((message :initform "Stringtable reader error")))

(defvar *stringtable-fallback-absorb-special* t
  "Should fallback functions absorb their special?

The default fallback listens to this variable.  User fallbacks
probably should as well.")

(defun fallback (special char delimiter from)
  (declare (ignore delimiter from))
  (if *stringtable-fallback-absorb-special*
      char
    (list special char)))

(defstruct (stringtable
            (:copier nil)
            (:print-object (lambda (object stream)
                             (print-unreadable-object (object stream :identity t)
                               (princ 'stringtable stream)))))
  (specials (list
             (list #\~ (make-hash-table) #'fallback)))
  (escape-character #\\))

;;; These are meant to be like the readtable functions, which is why
;;; their argument conventions kind of suck.
;;;

(defvar *stringtable* (make-stringtable)
  "The default current stringtable")

(defun copy-stringtable (&optional (from *stringtable*)
                                   (to nil)
                                   (nospecial nil))
  "Copy a stringtable

Make a copy of a stringtable: FROM if given is the stringtable to
copy, which by default is *STRINGTABLE*. If FROM is given as NIL then
a copy is made of the initial stringtable.  TO is the stringtable to
copy into, if given.  if not given a new stringtable is created.

If the third optional argument is not NIL, then don't copy special
character handlers.  In this case the only thing copied is the escape
character.

The argument convention is intentianally the same as COPY-READTABLE,
which is why it's slightly clunky, especially if you don't want any
specials."
  (flet ((cps (s)
           (if (not nospecial)
               (mapcar (lambda (e)
                         (list (first e)
                               (let ((n (make-hash-table)))
                                 (maphash (lambda (k v)
                                            (setf (gethash n k) v))
                                          (second e))
                                 n)
                               (third e)))
                       s)
             '())))
    (cond
     ((and from to)
      (setf (stringtable-specials to)
            (cps (stringtable-specials from))
            (stringtable-escape-character to)
            (stringtable-escape-character from))
      to)
     ((and from (not to))
      (make-stringtable
       :specials (cps (stringtable-specials from))
       :escape-character (stringtable-escape-character from)))
     (to
      ;; no from, use a secret constant to copy, remembering to pass
      ;; nospecial
      (copy-stringtable (load-time-value (make-stringtable) t) to nospecial))
     (nospecial
      ;; hacky special case (or nospecial case)
      (make-stringtable :specials '()))
     (t
      (make-stringtable)))))

(defun make-stringtable-special-character (character &optional
                                                     (fallback #'fallback)
                                                     (stringtable *stringtable*))
  "Make a special character in a stringtable

CHARACTER is the character to make special.  FALLBACK, if given, is
the fallback function for the special character: the default is a
function which simply returns the character following the special
character.  STRINGTABLE, if given, is the stringtable to modify: the
default is *STRINGTABLE*.  Return T.

This is intentionally similar to MAKE-DISPATCH-MACRO-CHARACTER."
  (check-type character character)
  (let ((entry (or (assoc character (stringtable-specials stringtable))
                   (push (list character nil nil)
                         (stringtable-specials stringtable)))))
    (setf (second entry) (make-hash-table)
          (third entry) fallback))
  t)

(defun get-stringtable-special-character (character subcharacter
                                                    &optional
                                                    (stringtable *stringtable*))
  "Get the handler function for a stringtable special character

For CHARACTER and SUBCHARACTER return the handler function in
STRINGTABLE (by default *STRINGTABLE*).  As a second value return true
if it is not the fallback, false if it is.

An error is signaled if CHARACTER is not a special character in the
stringtable."
  (let ((entry (assoc character (stringtable-specials stringtable))))
    (unless entry
      (error 'stringtable-error
             :format-control "not a special character"))
    (gethash subcharacter (second entry) (third entry))))

(defun set-stringtable-special-character (character subcharacter function
                                                    &optional
                                                    (stringtable *stringtable*))
  "Set the handler function for a stringtable special character

For CHARACTER and SUBCHARACTER set FUNCTION as the handler in
STRINGABLE (by default *STRINGTABLE*).  Return T.

An error is signaled if CHARACTER is not a special character in the
stringtable."
  (check-type subcharacter character)
  (let ((entry (assoc character (stringtable-specials stringtable))))
    (unless entry
      (error 'stringtable-error
             :format-control "~S isn't a special character"
             :format-arguments (list character)))
    (setf (gethash subcharacter (second entry)) function)
    t))

(defun read-string-with-stringtable (delimiter &optional
                                               (from *standard-input*)
                                               (stringtable *stringtable*))
  "Read a string with a stringtable

DELIMITER is the delimiter which terminates the string.  FROM is the
stream to read, by default *STANDARD-INPUT*, STRINGTABLE is the
stringtable to use, default *STRINGTABLE*.  Return the string."
  ;; This is meant to be like READ-DELIMITED-FORM.
  (coerce (read-string-with-stringtable/list delimiter from stringtable)
          'string))

(defun read-string-with-stringtable/list (delimiter from stringtable)
  (let ((escape (stringtable-escape-character stringtable))
        (specials (stringtable-specials stringtable)))
  (collecting
    (iterate inch ((this (read-char from t nil t))
                   (escaping nil)
                   (specially nil))
      (cond
       (escaping
        (collect this)
        (inch (read-char from t nil t) nil nil))
       (specially
        (when (char= this delimiter)
          (error 'stringtable-reader-error
                 :format-control "hit delimiter parsing special (~A~A)"
                 :format-arguments (list (first specially) this)
                 :stream from))
        (let ((result (funcall (gethash this (second specially)
                                        (third specially)) ;
                               (first specially) this delimiter from)))
          (typecase result
            (character
             (collect result))
            (list
             (dolist (c result)
               (collect c)))
            (string
             (dotimes (i (length result))
               (collect (char result i))))
            (t
             (error 'stringtable-reader-error
                    :format-control "what is ~S?" result
                    :format-arguments (list result)
                    :stream from)))
          (inch (read-char from t nil t) nil nil)))
       ((and escape (char= this escape))
        (inch (read-char from t nil t) t nil))
       ((assoc this specials)
        (inch (read-char from t nil t) nil (assoc this specials)))
       ((char= this delimiter))
       (t
        (collect this)
        (inch (read-char from t nil t) nil nil)))))))


;;; These two functions are perfecltly easy to implement for users,
;;; but they're the whole purpose of this, so they should exist.
;;;

(defun make-stringtable-readtable (&key (from *readtable*) (to nil)
                                        (delimiter #\"))
  "Make a readtable which uses a stringtable for some strings

FROM & TO specify readtables in the same way as COPY-READTABLE.

The stringtable will be attached to the DELIMITER subcharacter of the
# dispatching macro character.

Return the readtable."
  (let ((strt (copy-readtable from to)))
    (when (get-dispatch-macro-character #\# delimiter strt)
      (error "Someone is already using #~A" delimiter))
    (set-dispatch-macro-character
     #\# delimiter
     (lambda (stream char prefix)
       (declare (ignore prefix))
       (read-string-with-stringtable char stream))
     strt)
    strt))

(defun set-stringtable-newline-skipper (&key (stringtable *stringtable*)
                                             (special-character #\~)
                                             (white-warners t))
  ;; Not all the character names here are completely portable.
  "Set STRINGTABLE to have a newline skipper

Add a special handler for STRINGTABLE (default *STRINGTABLE*) which
will eat a newline and any number of whitespace characters.
SPECIAL-CHARACTER is the special character in STRINGTABLE, which is
#\~ by default.  If WHITE-WARNERS is true (by default) also add some
handlers for whitespace which merely warn.  This helps detect the case
of, for instance, '~ ' at the end of a line.  Return the stringtable."
  (flet ((newline-skipper (special this delimiter from)
           (declare (ignore special this delimiter))
           (do ((c (read-char from t nil t)
                   (read-char from t nil t)))
               ((not (member c '(#\Space
                                 #\Newline
                                 #\Tab #\Return #\Linefeed)))
                (progn
                  (unread-char c from)
                  nil))))
         (white-warner (special this delimiter from)
           (declare (ignore delimiter from))
           (warn "~A followed by ~(~A~): did you mean newline?"
                 special (char-name this))
           (if *stringtable-fallback-absorb-special*
               this
             (list special this))))
    (dolist (c '(#\Newline #\Return #\Linefeed))
      (set-stringtable-special-character
       special-character c #'newline-skipper stringtable))
    (when white-warners
      (dolist (c '(#\Space #\Tab))
        (set-stringtable-special-character
         special-character c #'white-warner stringtable))))
  stringtable)
