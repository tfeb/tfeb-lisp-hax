;;;; Some rudimentary tests for org.tfeb.hax.collecting
;;;

#+org.tfeb.tools.require-module
(org.tfeb.tools.require-module:needs
 :org.tfeb.hax.collecting
 #+Quicklisp
 ("parachute" :fallback ql:quickload))

(defpackage :org.tfeb.hax.collecting/test
  (:use :cl :org.tfeb.hax.collecting :org.shirakumo.parachute))

(in-package :org.tfeb.hax.collecting/test)

(define-test "org.tfeb.hax.collecting")

(define-test ("org.tfeb.hax.collecting" "collect-values")
  (is-values (with-collectors (a b)
               (is-values (collecting-values (a b) (values 1 2))
                 (eql 1)
                 (eql 2))
               (is-values (collecting-values (a b) 3 4)
                 (eql 3)
                 (eql 4)))
    (equal '(1 3))
    (equal '(2 4))))

(define-test ("org.tfeb.hax.collecting" "accumulators")
  (is = 1 (with-accumulators ((a +)) (a 1)))
  (is = 1 (with-accumulators ((a + :initially 1))))
  (is = 1 (with-accumulators ((a + :returner 1+))))
  (is = 1 (with-accumulators ((a + :by 1)) (a)))
  (is = 1 (with-accumulators ((a + :default 1)) (a)))
  (is = 1 (with-accumulators ((a + :default 1)) (a 1)))
  (is = 1 (with-accumulators ((a + :type fixnum)) (a 1))))

(define-test ("org.tfeb.hax.collecting" "collectors")
  (let ((c (make-collector)))
    (collect-into c 1)
    (collect-into c 2)
    (is equal '(1 2) (collector-contents c))
    (is equal '(1 2 3) (collector-contents c '(3))))
  (let ((c (make-collector)))
    (collect-into c 1)
    (is equal '(1 . 2) (collector-contents c 2)))
  (let ((c1 (make-collector))
        (c2 (make-collector))
        (c3 (make-collector)))
    (collect-into c1 1)
    (collect-into c2 2)
    (collect-into c3 3)
    (nconc-collectors c2 c3)
    (is equal '(3) (collector-contents c3))
    (is equal '(2 3) (collector-contents c2))
    (collect-into c3 4)                 ;c2's tail pointer is junk
    (is equal '(3 4) (collector-contents c3))
    (is equal '(2 3 4) (collector-contents c2))
    (nconc-collectors c1 c3)
    (is equal '(1 3 4) (collector-contents c1))
    (collect-into c3 5)                 ;c1's tail pointer is junk
    (is equal '(3 4 5) (collector-contents c3)))
  (let ((c (make-collector))
        (l (list 1 2 3)))
    (nconc-collector-onto c l)
    (is equal '(1 2 3) (collector-contents c))
    (collect-into c 4)
    (is equal '(1 2 3 4) (collector-contents c))
    (is equal '(1 2 3 4) l))
  (let ((c (make-collector))
        (l (list 1 2 3)))
    (collect-into c 0)
    (collect-into (nconc-collector-onto c l) 4)
    (is equal '(0 1 2 3 4) (collector-contents c))
    (is equal '(1 2 3 4) l))
  (let ((c (make-collector)))
    (true (collector-empty-p c))
    (collect-into c 1)
    (false (collector-empty-p c))
    (collect-into c 2)
    (is equal '(1 2) (collector-contents c))
    (is eql 1 (pop-collector c))
    (false (collector-empty-p c))
    (collect-into c 3)
    (is equal '(2 3) (collector-contents c))
    (is eql 2 (pop-collector c))
    (is eql 3 (pop-collector c))
    (true (collector-empty-p c))))

(define-test ("org.tfeb.hax.collecting" "vector-accumulators")
  ;; These are fairly limited
  (is eq
      (upgraded-array-element-type 'character)
      (array-element-type (with-vector-accumulators ((s :element-type 'character)))))
  (let ((s (make-string 2)))
    ;; Test that it does not gratuitously copy the vector
    (is eq s (with-vector-accumulators ((s s))
               (s #\a)
               (s #\b)))
    (is string= "ab" s)
    ;; but it needs to here, and it will mutate s as well
    (let ((r (with-vector-accumulators ((s s))
               (s #\x)
               (s #\y)
               (s #\z))))
      (isnt eq s r)
      (is = 3 (length r))
      (is string= "xyz" r)
      (is string= "xy" s)
      (is eq (array-element-type s) (array-element-type r))))
  ;; Check extending works
  (let* ((v (make-array 0 :adjustable t))
         (r (with-vector-accumulators ((v v)) (v 1))))
    (is eq v r)
    (is = 1 (length v)))
  (let ((v (make-array 2)))
    ;; This will fail
    (fail (with-vector-accumulators ((v v :start 10)) (v 1)))
    ;; But this is oddly OK as it extends by 1: should this be fixed?
    (finish (with-vector-accumulators ((v v :start 2)) (v 1))))
  ;; Check we can write into the middle of a vector and not reallocate
  ;; it or prune it
  (let* ((v (make-array 10 :initial-element 1))
         (r (with-vector-accumulators ((v v :start 4 :finalize nil)) (v 2))))
    (is eq v r)
    (is = 10 (length r))
    (is = 2 (aref r 4))))


(let ((result (test "org.tfeb.hax.collecting" :report 'summary)))
  (when (eq (status result) ':failed)
    (error "Tests for collecting failed" result))
  result)
