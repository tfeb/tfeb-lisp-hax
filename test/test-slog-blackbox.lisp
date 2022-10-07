;;;; Black box testing of slog
;;;
;;; This is currently fairly rudementary
;;;

(in-package :org.tfeb.hax.slog/test/blackbox)

(define-test "org.tfeb.hax.slog/blackbox"
  :parent (:org.tfeb.hax.slog/test "org.tfeb.hax.slog"))

(define-condition simple-once-only-log-entry (simple-log-entry once-only-log-entry)
  ())

(define-test ("org.tfeb.hax.slog/blackbox" "sanity")
  ;; Essential sanity checks
  (close-open-log-files :all t :reset t)
  (multiple-value-bind (opened closed) (current-log-files :all t)
    (is = 0 (length opened))
    (is = 0 (length closed)))
  (let ((lf1 (load-relative-pathname "log/foo.log"))
        (lf2 (load-relative-pathname "log/bar.log")))
    (logging ((t lf1))
      (logging ((t lf2))
        (multiple-value-bind (opened closed) (current-log-files)
          ;; This is a very weak test: really I just want to check the
          ;; function does not fall about.
          (true (log-files-sane-p opened closed :warn t)))))
    (multiple-value-bind (opened closed) (current-log-files :all t)
      (is = 0 (length opened))
      (is = 0 (length closed)))
    (ensure-deleted lf1 lf2)))

(define-test ("org.tfeb.hax.slog/blackbox" "once-only")
  ;; Tests of once-only logging and file opening
  :depends-on ("sanity")
  (let ((lf1 (load-relative-pathname "log/foo.log"))
        (lf2 (load-relative-pathname "log/bar.log")))
    (ensure-deleted lf1 lf2)
    ;; Neither file should exist now
    (false (probe-file lf1))
    (false (probe-file lf2))
    (logging ((t lf1 lf2))
      (multiple-value-bind (opened closed) (current-log-files)
        ;; Both files will now be open since logging opens them
        (is = 2 (length opened))
        (is = 0 (length closed)))
      (close-open-log-files)
      (multiple-value-bind (opened closed) (current-log-files)
        ;; Now both files should be closed
        (is = 0 (length opened))
        (is = 2 (length closed)))
      (slog 'simple-once-only-log-entry
            :format-control "once-only")
      ;; one file will now have been reopened
      (multiple-value-bind (opened closed) (current-log-files)
        (is = 1 (length opened))
        (is = 1 (length closed))))
    ;; Both files should exist
    (true (probe-file lf1))
    (true (probe-file lf2))
    (ensure-deleted lf1 lf2)
    ;; And now be gone
    (false (probe-file lf1))
    (false (probe-file lf2))
    (logging ((t lf1 lf2))
      (multiple-value-bind (opened closed) (current-log-files)
        ;; Both will be open again
        (is = 2 (length opened))
        (is = 0 (length closed)))
      (close-open-log-files)
      (multiple-value-bind (opened closed) (current-log-files)
        ;; Both will now be closed
        (is = 0 (length opened))
        (is = 2 (length closed)))
      (slog 'simple-log-entry
            :format-control "simple")
      (multiple-value-bind (opened closed) (current-log-files)
        ;; Both will now be open again
        (is = 2 (length opened))
        (is = 0 (length closed))))
    (multiple-value-bind (opened closed) (current-log-files)
      ;; There are now no current log files
      (is = 0 (length opened))
      (is = 0 (length closed)))
    ;; Both should exist
    (true (probe-file lf1))
    (true (probe-file lf2))
    (ensure-deleted lf1 lf2)))

(define-test ("org.tfeb.hax.slog/blackbox" "closing-tests")
  ;; Test that we can close just some files
  :depends-on ("sanity")
  (let ((lf1 (load-relative-pathname "log/foo.log"))
        (lf2 (load-relative-pathname "log/bar.log")))
    (logging ((t lf1 lf2))
      (multiple-value-bind (opened closed) (current-log-files)
        ;; Both will be open
        (is = 2 (length opened))
        (is = 0 (length closed)))
      (close-open-log-files :test (lambda (p)
                                    (string= (pathname-name p)
                                                  (pathname-name lf1))))
      (multiple-value-bind (opened closed) (current-log-files)
        ;; One should now be open
        (is = 1 (length opened))
        (is = 1 (length closed))
        (is string= (pathname-name (first closed)) (pathname-name lf1))
        (is string= (pathname-name (first opened)) (pathname-name lf2))))
    (ensure-deleted lf1 lf2)))

(define-test ("org.tfeb.hax.slog/blackbox" "logging-binding")
  :depends-on ("sanity")
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
