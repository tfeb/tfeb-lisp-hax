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
 (:org.tfeb.tools.feature-expressions :compile t)
 (:org.tfeb.hax.metatronic :compile t))

(defpackage :org.tfeb.hax.slog
  (:use :cl
   :org.tfeb.hax.simple-loops :org.tfeb.hax.collecting
   :org.tfeb.hax.spam
   :org.tfeb.tools.feature-expressions
   :org.tfeb.hax.metatronic)
  #+ASDF
  (:import-from "UIOP" #:getcwd)
  (:export
   #:log-entry
   #:log-entry-internal-time
   #:once-only-log-entry
   #:simple-log-entry
   #:slog
   #:closing-opened-log-files
   #:current-log-files
   #:close-open-log-files
   #:flush-open-log-file-streams
   #:get-precision-universal-time
   #:default-log-entry-formatter
   #:*log-entry-formatter*
   #:slog-to
   #:*fallback-log-destination-handler*
   #:logging))

(in-package :org.tfeb.hax.slog)

(provide :org.tfeb.hax.slog)

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

#-ASDF
(defun getcwd ()
  ;; A fallback implementation of getcwd for a couple of
  ;; implementations
  #+LispWorks
  (hcl:get-working-directory)
  #+SBCL
  (pathname (sb-unix:posix-getcwd/))
  #+Clozure
  (ccl:current-directory)
  #-(or LispWorks)
  (make-pathame :name nil :type nil :version nil
                :defaults *default-pathname-defaults*))

(defun canonicalize-destination (dest &optional (cwd nil cwdp))
  ;; This gets called both by the expansion of LOGGING and by then
  ;; again by LOG-FILE-STREAM, so it tries hard to avoid consing
  ;; pathnames it does not need to cons.
  (typecase dest
    (pathname
     (let ((d (pathname-directory dest)))
       (if (and (listp d) (eq (first d) ':absolute))
           dest
         (merge-pathnames dest (if cwdp cwd (getcwd))))))
    (string
     (let* ((p (pathname dest))
            (d (pathname-directory p)))
       (if (and (listp d) (eq (first d) ':absolute))
           p
         (merge-pathnames p (if cwdp cwd (getcwd))))))
    (t dest)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (feature-case
    ((and (not :ASDF)
          (not (or :LispWorks :SBCL :Clozure)))
     (warn "using hopeless fallback for getcwd"))))

(defvar *log-file-streams*
  ;; Maps from absolute pathnames to streams
  '())

(defvar *slfs*
  ;; saved tail of *log-file-streams*
  ())

(defun log-file-stream (p)
  ;; Given a pathname, return a stream open for writing to its
  ;; absolute name.
  (typecase p
    ((or pathname string)
     (flet ((ensure-open-stream (entry)
              (destructuring-bind (f . s) entry
                (if (and (streamp s) (open-stream-p s))
                    s
                (progn
                  (ensure-directories-exist f)
                  (setf (cdr entry)
                        (open f
                              :direction ':output
                              :if-exists ':append
                              :if-does-not-exist ':create)))))))
       (let* ((ap (canonicalize-destination p))
              (apm (assoc ap *log-file-streams* :test #'equal)))
         (if apm
             (ensure-open-stream apm)
           (let ((entry (cons ap nil)))
             (push entry *log-file-streams*)
             (ensure-open-stream entry))))))
    (t
     (error "Mutant type ~S: no idea how to canonicalize this to a filename"
            (type-of p)))))

(defun close-open-log-files-up-to (up-to &key (abort nil))
  ;; Close any open log files up to a specified tail.  Returns the
  ;; names of the closed files as well as any which were already
  ;; closed.  Does not set *log-file-streams*
  (with-collectors (opened closed)
    (do ((tail *log-file-streams* (cdr tail)))
        ((or (eq tail up-to) (null tail)))
      (destructuring-bind (filename . stream/canonical) (first tail)
        (etypecase stream/canonical
          (stream
           (if (open-stream-p stream/canonical)
               (progn
                 (close stream/canonical :abort abort)
                 (opened filename))
             (closed filename)))
          (pathname))))))

(defun current-log-files (&key (all nil))
  ;; Return lists of open and closed log files.  By default back to
  ;; the last saved state, with all return all of them
  (with-collectors (opened closed)
    (do ((clft *log-file-streams* (cdr clft)))
        ((if all (null clft) (eq clft *slfs*)))
      (destructuring-bind (filename . stream/canonical) (first clft)
        (etypecase stream/canonical
          (stream
           (if (open-stream-p stream/canonical)
               (opened filename)
             (closed filename)))
          (pathname))))))

(defun close-open-log-files (&key (all nil) (abort nil)
                                  (reset nil))
  ;; Normally just close current the open log files.  With reset reset
  ;; the current list (only to the last saved state)
  (let ((clft (if all nil *slfs*)))
    (multiple-value-prog1
        (close-open-log-files-up-to clft :abort abort)
      (when reset
        (setf *log-file-streams* clft)))))

(defun flush-open-log-file-streams (&key (all nil)
                                         (wait nil))
  (collecting
    (do ((clft *log-file-streams* (cdr clft)))
        ((if all (null clft) (eq clft *slfs*)))
      (destructuring-bind (filename . stream/canonical) (first clft)
        (etypecase stream/canonical
          (stream
           (when (open-stream-p stream/canonical)
             (if wait
                 (finish-output stream/canonical)
               (force-output stream/canonical))
             (collect filename)))
          (pathname))))))

(defun call/closing-opened-log-files (f &key (abort nil)
                                        (reporter nil))
  (let ((*slfs* *log-file-streams*)
        (*log-file-streams* *log-file-streams*))
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

(defmethod slog-to ((to pathname) (datum log-entry) &key)
  (slog-to (log-file-stream to) datum))

(defmethod slog-to ((to string) (datum log-entry) &key)
  (slog-to (log-file-stream to) datum))

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
