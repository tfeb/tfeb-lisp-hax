;;;; Run the tests for slog
;;;

(in-package :org.tfeb.hax.slog/test)

(let ((result (test "org.tfeb.hax.slog" :report 'summary)))
  (when (eq (status result) ':failed)
    (error "Tests for slog failed" result))
  result)
