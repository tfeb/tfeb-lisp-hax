;;;; White box testing of slog
;;;

(in-package :org.tfeb.hax.slog/test/whitebox)

(define-test "org.tfeb.hax.slog/whitebox"
  :parent (:org.tfeb.hax.slog/test "org.tfeb.hax.slog"))

(defun load-relative-pathname (p)
  (if *load-truename*
      (merge-pathnames (pathname p)
                       *load-truename*)
    p))

(define-test ("org.tfeb.hax.slog/whitebox" "sanity")
  (true (null *log-file-streams*))
  (let ((lf (load-relative-pathname "log/foo.log")))
    (when (probe-file lf) (delete-file lf))
    (false (probe-file lf))
    (logging ((t lf))
      (slog "test"))
    (true (probe-file lf))
    (true (null *log-file-streams*))
    (when (probe-file lf) (delete-file lf))))

(define-test ("org.tfeb.hax.slog/whitebox" "reopen")
  (let ((lf (load-relative-pathname "log/foo.log")))
    (logging ((t lf))
      (slog "test 1")
      (is = 1 (length *log-file-streams*))
      (close-open-log-files :reset t)
      (is = 0 (length *log-file-streams*))
      (slog "test 2")
      (is = 1 (length *log-file-streams*)))
    (is = 0 (length *log-file-streams*))
    (finish (delete-file lf))))

(defun it-offset ()
  (multiple-value-bind (ut it)
      (compute-image-time-offsets)
    (- (* ut internal-time-units-per-second) it)))

(define-test ("org.tfeb.hax.slog/whitebox" "precision-time")
  ;; A version of this is also in the source as basic sanity
  ;;
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
