;;;; Black box testing of slog
;;;

(in-package :org.tfeb.hax.slog/test/blackbox)

(define-test "org.tfeb.hax.slog/blackbox"
  :parent (:org.tfeb.hax.slog/test "org.tfeb.hax.slog"))

(defun load-relative-pathname (p)
  (if *load-truename*
      (merge-pathnames (pathname p)
                       *load-truename*)
    p))

(defun ensure-deleted (&rest files)
  (dolist (file files)
    (when (probe-file file)
      (delete-file file))))

(define-condition simple-once-only-log-entry (simple-log-entry once-only-log-entry)
  ())

(define-test ("org.tfeb.hax.slog/blackbox" "once-only")
  (let ((lf1 (load-relative-pathname "log/foo.log"))
        (lf2 (load-relative-pathname "log/bar.log")))
    (ensure-deleted lf1 lf2)
    (false (probe-file lf1))
    (false (probe-file lf2))
    (logging ((t lf1 lf2))
      (slog 'simple-once-only-log-entry
            :format-control "foo")
      (multiple-value-bind (opened closed) (current-log-files)
        (is = (length opened) 1)
        (is = (length closed) 0)))
    (true (probe-file lf1))
    (false (probe-file lf2))
    (ensure-deleted lf1 lf2)
    (false (probe-file lf1))
    (false (probe-file lf2))
    (logging ((t lf1 lf2))
      (slog 'simple-log-entry
            :format-control "foo")
      (multiple-value-bind (opened closed) (current-log-files)
        (is = (length opened) 2)
        (is = (length closed) 0)))
    (true (probe-file lf1))
    (true (probe-file lf2))))

(define-test ("org.tfeb.hax.slog/blackbox" "logging-binding")
  (let ((log-destination nil))
    (true (zerop (length (with-output-to-string (o)
                           (logging ((t log-destination))
                             (slog "foo")
                             (setf log-destination o)
                             (slog "bar")))))))
  (false (zerop (length (with-output-to-string (o)
                          (let ((o2 o))
                            (logging ((t o2))
                              (setf o2 nil)
                              (slog "foo"))))))))
