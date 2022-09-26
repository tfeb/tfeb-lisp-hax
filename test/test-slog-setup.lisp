;;;; Setup tests for slog
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
  (:org.tfeb.hax.slog :compile t)
  (:org.tfeb.conduit-packages :compile t)
  ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.slog/test
  (:use :cl
   :org.shirakumo.parachute))

(defpackage :org.tfeb.hax.slog/test/blackbox
  (:use :cl
   :org.tfeb.hax.slog
        :org.shirakumo.parachute))

(org.tfeb.clc:defpackage :org.tfeb.hax.slog/test/whitebox
  (:clones :org.tfeb.hax.slog)
  ;; IS clashes
  (:shadowing-import-from :org.shirakumo.parachute #:is)
  (:use :org.shirakumo.parachute))

(in-package :org.tfeb.hax.slog/test)

(define-test "org.tfeb.hax.slog")
