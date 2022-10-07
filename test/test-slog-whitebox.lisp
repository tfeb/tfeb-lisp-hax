;;;; White box testing of slog
;;;
;;; This is currently rudementary: many more tests are needed around
;;; the management of log-file objects.
;;;

(in-package :org.tfeb.hax.slog/test/whitebox)

(define-test "org.tfeb.hax.slog/whitebox"
  :parent (:org.tfeb.hax.slog/test "org.tfeb.hax.slog"))

(define-test ("org.tfeb.hax.slog/whitebox" "sanity")
  (close-open-log-files :all t :reset t)
  (true (null *log-files*))
  (let ((lf (load-relative-pathname "log/foo.log")))
    (ensure-deleted lf)
    (false (probe-file lf))
    (logging ((t lf))
      (slog "test"))
    (true (probe-file lf))
    (true (null *log-files*))
    (ensure-deleted lf)))

(define-test ("org.tfeb.hax.slog/whitebox" "log-files")
  ;; Rudimentary log-file tests
  :depends-on ("sanity")
  (let* ((f (load-relative-pathname "log/foo.log"))
         (lf1 (ensure-log-file f))
         (lf2 (ensure-log-file (namestring f)))
         (lf3 (ensure-log-file lf2)))
    (is = 1 (length *log-files*))
    (is eq lf1 lf2)
    (is eq lf1 lf3)
    (true (log-file-open-p lf1))
    (close-log-file lf1)
    (false (log-file-open-p lf1))
    (close-open-log-files :all t :reset t)
    (true (null *log-files*))
    (ensure-deleted f)))

(define-test ("org.tfeb.hax.slog/whitebox" "reopen")
  ;; Test reopening (not sure why this is here)
  :depends-on ("sanity")
  (let ((lf (load-relative-pathname "log/foo.log")))
    (logging ((t lf))
      (slog "test 1")
      (is = 1 (length *log-files*))
      (close-open-log-files :reset t)
      (is = 0 (length *log-files*))
      (slog "test 2")
      (is = 1 (length *log-files*)))
    (is = 0 (length *log-files*))
    (finish (delete-file lf))))

(defun it-offset ()
  (multiple-value-bind (ut it)
      (compute-image-time-offsets)
    (- (* ut internal-time-units-per-second) it)))

(define-test ("org.tfeb.hax.slog/whitebox" "precision-time")
  ;; A version of this is also in the source as basic sanity
  (logging ((t *error-output*))
    (let ((goods 0)
          (stepped 0)
          (bads 0)
          (trials 1000000))
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
      (is = bads 0
          "from ~D tries ~D precision times aren't" trials bads)
      (false (zerop goods)
             "no good results from ~D trials" trials)
      (is < 1/100 (/ stepped goods)
          "from ~D trials got ~D good times, but ~D stepped"
          trials goods stepped))))
