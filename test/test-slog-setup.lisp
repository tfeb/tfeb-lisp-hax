;;;; Setup tests for slog
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
  (:org.tfeb.hax.slog :compile t)
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.slog/test
  (:use :cl
   :org.shirakumo.parachute)
  (:export
   #:load-relative-pathname
   #:ensure-deleted))

(defpackage :org.tfeb.hax.slog/test/blackbox
  (:use :cl
   :org.tfeb.hax.slog
   :org.tfeb.hax.slog/test
   :org.shirakumo.parachute))

(defpackage :org.tfeb.hax.slog/test/whitebox
  (:use :cl
   :org.tfeb.hax.slog
      :org.tfeb.hax.slog/test
   :org.shirakumo.parachute)
  (:import-from :org.tfeb.hax.slog
   ;; Anything needed for white box tests should go here
   #:*log-files*
   #:log-file #:log-file-pathname
   #:ensure-log-file
   #:log-file-open-p
   #:close-log-file
   #:compute-image-time-offsets))

(in-package :org.tfeb.hax.slog/test)

(define-test "org.tfeb.hax.slog")

(defun load-relative-pathname (p)
  (if *load-truename*
      (merge-pathnames (pathname p)
                       *load-truename*)
    p))

(defun ensure-deleted (&rest files)
  (dolist (file files)
    (when (probe-file file)
      (delete-file file))))
