;;;; Simple logging
;;;

;;; The basic trick here is that signalling a condition can just be a
;;; way of saying 'hey, an interesting thing happened', and handlers
;;; can just do something with a signalled condition and pass it on
;;; ... which is what logging is.
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 (:org.tfeb.hax.simple-loops :compile t)
 (:org.tfeb.hax.collecting :compile t)
 (:org.tfeb.hax.spam :compile t)
 (:org.tfeb.hax.metatronic :compile t))

(defpackage :org.tfeb.hax.slog
  (:use :cl
   :org.tfeb.hax.simple-loops :org.tfeb.hax.collecting
   :org.tfeb.hax.spam
   :org.tfeb.hax.metatronic)
  (:export
   #:log-entry
   #:log-entry-internal-time
   #:once-only-log-entry
   #:simple-log-entry
   #:slog
   #:closing-opened-log-files
   #:current-log-files
   #:log-files-sane-p
   #:close-open-log-files
   #:flush-open-log-files
   #:get-precision-universal-time
   #:default-log-entry-formatter
   #:*log-entry-formatter*
   #:slog-to
   #:*fallback-log-destination-handler*
   #:logging))

(in-package :org.tfeb.hax.slog)

(provide :org.tfeb.hax.slog)

;;; log-entries are the conditions signalled to cause logging to happen
;;;

(define-condition log-entry (condition)
  ((internal-time :initform (get-internal-real-time)
                  :reader log-entry-internal-time))
  (:documentation "all SLOG condition types inherit from this"))

;;; Conditions are meant to be immutable, hence this little dance.
;;;

(define-condition once-only-log-entry (log-entry)
  ((logged :initform (cons nil nil)
           :reader log-entry-logged))
  (:documentation "class of log entries which are logged once only"))

(defgeneric log-entry-logged-p (log-entry)
  (:method ((log-entry log-entry))
   nil)
  (:method ((log-entry once-only-log-entry))
   (car (log-entry-logged log-entry))))

(defgeneric (setf log-entry-logged-p) (new log-entry)
  (:method (new (log-entry once-only-log-entry))
   (setf (car (log-entry-logged log-entry)) new)))

(define-condition simple-log-entry (log-entry simple-condition)
  ()
  (:documentation "simple SLOG condition"))

(defun ensure-log-entry (datum arguments)
  (typecase datum
    (string
     (make-condition 'simple-log-entry
                     :format-control datum
                     :format-arguments arguments))
    (log-entry
     datum)
    (t
     (apply #'make-condition datum arguments))))

(defun slog (datum &rest arguments)
  (signal (ensure-log-entry datum arguments)))

;;; log-files are objects which wrap a pathname and a stream for which
;;; the pathname is the truename.
;;;
;;; It is tempting to have a log-file maintain a notion of whether it
;;; is 'registered' -- on the *log-files* stack, which would allow
;;; ensure-log-file to avoid checking and registering it.  This is not
;;; safe at least in the presence of the reset option to
;;; close-open-log files, but this could be worked around by having it
;;; unset the registered flag on any log-files.  On the other hand it
;;; seems like a lot of work to make things perhaps less safe, given
;;; that there probably are never going to be more than a rather small
;;; number of entries on *log-files*.  So for now, I will leave it.
;;;

(defstruct (log-file
            (:print-function (lambda (e s d)
                               (declare (ignore d))
                               (print-unreadable-object (e s :type t :identity t)
                                 (format s "~S (~:[closed~;open~])"
                                         (log-file-pathname e)
                                         (log-file-stream e)))))
            (:copier nil))
  (pathname (error "hopeless botch")
            :read-only t)
  (stream (error "hopeless botch")))

(defun log-file-open-p (log-file)
  (if (log-file-stream log-file) t nil))

(defun close-log-file (log-file &key (abort nil))
  (prog1 (close (log-file-stream log-file) :abort abort)
    (setf (log-file-stream log-file) nil)))

(defvar *log-files*
  ;; Current log files
  '())

(defvar *slfs*
  ;; saved tail of *log-files*
  ())

(defun open-log-file-pathname (pathname)
  ;; Get all the options in one place
  (ensure-directories-exist pathname)
  (open pathname
        :direction ':output
        :if-does-not-exist ':create
        :if-exists ':append))

;;; Ensuring a log file will create one and also make sure it is
;;; registered properly.  Anything that wants to log to a file should
;;; call this.

(defgeneric ensure-log-file (thing &key open))

(defmethod ensure-log-file ((lfn string) &key (open nil))
  (ensure-log-file (pathname lfn) :open open))

(defmethod ensure-log-file ((lfn pathname) &key (open nil))
  (let ((elf (find (or (probe-file lfn) lfn)
                   *log-files* :test #'equal :key #'log-file-pathname)))
    (cond
     (elf
      (when open
        (unless (log-file-open-p elf)
          (setf (log-file-stream elf)
                (open-log-file-pathname lfn))))
      elf)
     (t
      (let* ((s (open-log-file-pathname lfn))
             (nlf (make-log-file :pathname (truename s)
                                 :stream s)))
        (push nlf *log-files*)
        nlf)))))

(defmethod ensure-log-file ((lf log-file) &key (open nil))
  (pushnew lf *log-files*)              ;ensure it is registered
  (when open
    (unless (log-file-stream lf)
      (setf (log-file-stream lf)
            (open-log-file-pathname (log-file-pathname lf)))))
  lf)

(defun close-open-log-files-up-to (up-to &key (abort nil) (test nil))
  ;; Close any open log files up to a specified tail.  With test
  ;; return only those whose pathnames pass the test.  Returns the
  ;; names of the closed files as well as any which were already
  ;; closed.  Does not set *log-files*
  (with-collectors (opened closed)
    (do ((tail *log-files* (cdr tail)))
        ((or (eq tail up-to) (null tail)))
      (let ((lf (car tail)))
        (if (and (log-file-open-p lf)
                 (or (not test) (funcall test (log-file-pathname lf))))
            (progn
              (close-log-file lf :abort abort)
              (opened (log-file-pathname lf)))
          (closed (log-file-pathname lf)))))))

(defun current-log-files (&key (all nil))
  ;; Return lists of open and closed log file names.  By default back
  ;; to the last saved state, with all return all of them.
  (with-collectors (opened closed)
        (do ((clft *log-files* (cdr clft)))
            ((if all (null clft) (eq clft *slfs*)))
          (let* ((lf (car clft))
                 (p (log-file-pathname lf)))
            (if (log-file-open-p lf)
                (opened p)
              (closed p))))))

(defun log-files-sane-p (opened closed &key (warn nil) (error nil))
  ;; Sanity check the two lists.  With warn report what's wrong.  With
  ;; error signal an error is insane.  If this fails it's a bug in slog.
  (let ((sane t))
    (mapl (lambda (ot)
            (destructuring-bind (o . ott) ot
              (when (member o ott :test #'equal)
                (when warn (warn "~A is open multiple times" o))
                (setf sane nil))
              (when (member o closed :test #'equal)
                (when warn (warn "~A is both open and closed" o))
                (setf sane nil))))
          opened)
    (mapl (lambda (ct)
            (destructuring-bind (c . ctt) ct
              (when (member c ctt :test #'equal)
                (when warn (warn "~A is closed multiple times" c))
                (setf sane nil))))
          closed)
    (when (and error (not sane))
      (error "log files are not sane"))
    sane))

(defun close-open-log-files (&key (all nil) (abort nil)
                                  (test nil) (reset nil))
  ;; Normally just close the current open log files.  if TEST is given
  ;; close only those whose pathnames pass the test. With reset reset
  ;; the current list (only to the last saved state).  You can't give
  ;; both test and reset this would leak filehandles.
  (when (and test reset)
    (error "can't give both test and reset"))
  (let ((clft (if all nil *slfs*)))
    (multiple-value-prog1
        (close-open-log-files-up-to clft :abort abort :test test)
      (when reset
        (setf *log-files* clft)))))

(defun flush-open-log-files (&key (all nil)
                                         (wait nil))
  (collecting
    (do ((clft *log-files* (cdr clft)))
        ((if all (null clft) (eq clft *slfs*)))
      (let ((lf (car clft)))
        (when (log-file-open-p lf)
          (if wait
              (finish-output (log-file-stream lf))
            (force-output (log-file-stream lf)))
          (collect (log-file-pathname lf)))))))

(defun call/closing-opened-log-files (f &key (abort nil)
                                        (reporter nil))
  (let ((*slfs* *log-files*)
        (*log-files* *log-files*))
    (unwind-protect
        (funcall f)
      (if reporter
          (map nil reporter (close-open-log-files-up-to *slfs* :abort abort))
        (close-open-log-files-up-to *slfs* :abort abort)))))

(defmacro closing-opened-log-files ((&key (abort nil) (reporter nil)) &body forms)
  `(call/closing-opened-log-files
    (lambda ()
      ,@forms)
    :abort ,abort :reporter ,reporter))

;;; Precision universal time
;;;

(defun compute-image-time-offsets (&optional (tries 3))
  ;; Return a universal time and the internal time at the point it
  ;; ticked.  This necessarily takes more than a second.
  (looping ((try 1))
    (cond
     ((> try tries)
      (error "time is out of joint"))
     ((> try 1)
      (warn "hours pass like seconds")))
    (escaping (retry)
      (let ((start (get-universal-time)))
        (doing ((now (get-universal-time)))
               ((> now start)
                (unless (= now (1+ start))
                  (retry (1+ try))))))
      (let ((start (get-universal-time)))
        (doing ((count 1 (1+ count))
                (ib (get-internal-real-time))
                (now (get-universal-time))
                (ia (get-internal-real-time)))
               ((> now start)
                ;; clock has ticked
                (when (> now (1+ start))
                  (warn "time moves slowly to its end")
                  (retry (1+ try)))
                (return-from compute-image-time-offsets
                  ;; Just average the two internal times we got to try and
                  ;; get a reasonable offset
                  (list now (round (+ ib ia) 2)))))))))

(defconstant default-precision-time-rate
  (min internal-time-units-per-second 1000))

(defun get-precision-universal-time (&key
                                     (it (get-internal-real-time))
                                     (type 'rational)
                                     (rate default-precision-time-rate ratep)
                                     (chide nil))
  ;; Return two values: the most precise idea of the time we can work
  ;; out, and the number of significant decimal places (which is just
  ;; (log rate 10)
  (when chide
    (case type
      ((single-float short-float)
       (warn "~S is almost certainly not precise enough to be useful"
             type)))
    (when (> rate internal-time-units-per-second)
      (warn "rate ~D is greater thant internal clock rate ~D"
            rate internal-time-units-per-second)))
  (destructuring-bind (ut0 it0) (load-time-value (compute-image-time-offsets))
    (let ((pt (+ ut0 (/ (round (* rate (- it it0)) internal-time-units-per-second)
                        rate))))
      (values
       (ecase type
         ((rational ratio) pt)
         ((float double-float ) (* pt 1.0d0))
         ((long-float) (* pt 1.0l0))
         ((single-float) (* pt 1.0f0))
         ((short-float) (* pt 1.0s0)))
       rate
       (cond
        ((not ratep)
         (load-time-value (ceiling (log default-precision-time-rate 10))))
        ((= rate internal-time-units-per-second)
         (load-time-value (ceiling (log internal-time-units-per-second 10))))
        (t
         (ceiling (log rate 10))))))))

(defun default-log-entry-formatter ()
  (lambda (to log-entry)
    (multiple-value-bind (seconds rate decimal-places)
        (get-precision-universal-time :it (log-entry-internal-time log-entry)
                                      :type 'double-float)
      (declare (ignore rate))
      (format to "~&~,VF ~A~%" decimal-places seconds log-entry))))

(defvar *log-entry-formatter* (default-log-entry-formatter))

;;; I am not sure how extensible slog-to should be
;;;

(defgeneric slog-to (to datum &key)
  (:argument-precedence-order datum to))

(defmethod slog-to (to datum &rest arguments &key &allow-other-keys)
  (slog-to to (ensure-log-entry datum arguments)))

(defmethod slog-to ((to stream) (datum log-entry) &key)
  (funcall *log-entry-formatter* to datum)
  datum)

(defmethod slog-to ((to log-file) (datum log-entry) &key)
  (slog-to (log-file-stream (ensure-log-file to :open t))
           datum))

(defmethod slog-to ((to pathname) (datum log-entry) &key)
  (slog-to (log-file-stream (ensure-log-file to :open t)) datum))

(defmethod slog-to ((to string) (datum log-entry) &key)
  (slog-to (log-file-stream (ensure-log-file to :open t)) datum))

(defmethod slog-to ((to function) (datum log-entry)
                    &rest arguments &key &allow-other-keys)
  (apply to datum arguments))

(defmethod slog-to ((to symbol) (datum log-entry)
                    &rest arguments &key &allow-other-keys)
  (apply (symbol-function to) datum arguments))

(defmethod slog-to ((to null) (datum log-entry) &key)
  (declare (ignore to))
  datum)

(defvar *fallback-log-destination-handler* nil)

(defmethod slog-to (to (datum log-entry)
                       &rest arguments &key &allow-other-keys)
  (if *fallback-log-destination-handler*
      (apply *fallback-log-destination-handler* to datum arguments)
    (error "no fallback log destination handler")))

(defmethod slog-to :around (to (datum once-only-log-entry) &key)
  (unless (log-entry-logged-p datum)
    (multiple-value-prog1
        (call-next-method)
      (setf (log-entry-logged-p datum) t))))

;;; logging macro and its immediate support
;;;

(defun canonicalize-destination (destination)
  ;; This is what knows that pathnames and strings designate files,
  ;; and ensures log-file objects for them
  (typecase destination
    ((or pathname string)
     (ensure-log-file destination))
    (t destination)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; Needed on voyage
  (defun ensure-log-entry-typespec (typespec)
    (etypecase typespec
      (symbol
       (case typespec
         ((t)
          'log-entry)
         (otherwise
          (unless (subtypep typespec 'log-entry)
            (error "~S doesn't look like a log-entry subtype" typespec))
          typespec)))
      (cons
       (cons (first typespec)
             (mapcar #'ensure-log-entry-typespec (rest typespec)))))))

(defmacro/m logging (clauses &body forms)
  (unless (matchp clauses (list-of (cons-matches (any) (list-of (any)))))
    (error "logging clauses ~S aren't" clauses))
  `(closing-opened-log-files ()
     (handler-bind
         ,(collecting
            (dolist (clause clauses)
              (destructuring-bind (typespec . destinations) clause
                (let ((bindings (collecting
                                  (dolist (d destinations)
                                    (collect
                                     `(,(gensym) (canonicalize-destination ,d)))))))
                  (collect
                   `(,(ensure-log-entry-typespec typespec)
                     (let ,bindings
                       (lambda (<log-entry>)
                         ,@(collecting
                             (dolist (binding bindings)
                               (collect
                                `(slog-to ,(first binding)
                                          <log-entry>))))))))))))
       ,@forms)))


;;; Some tests
;;;

(logging ((t *error-output*))
  (let ((goods 0)
        (stepped 0)
        (bads 0)
        (trials 10000))
    (dotimes (i trials)
      (let* ((integer (get-universal-time))
             (precision (get-precision-universal-time :type 'double-float))
             (floored (floor precision)))
        (cond
         ((= floored integer)
          (incf goods))
         ((= floored (1+ integer))
          (slog "precision time ~F is stepped from ~D"
                precision integer)
          (incf stepped))
         (t
          (slog "precision time ~F is hopelessly different than ~D"
                precision integer)
          (incf bads)))))
    (when (> stepped 0)
        (slog "~D stepped from ~D trials" stepped trials))
    (when (> bads 0)
      (warn "from ~D tries ~D precision times aren't" trials bads))
    (when (or (zerop goods)
              (> (/ stepped goods) 1/100))
      (warn "from ~D trials got ~D good times, but ~D stepped"
            trials goods stepped))))
