#lang racket

;;; Copyright (c) Kirk Rader 2026

(require rackunit
         "timer.rkt"
         "engines.rkt"
         "concurrent-or.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit test for engines, concurrent-or, and timers

(test-begin
 (call-with-values
  (lambda ()
    (let* ((counts (make-hash))
           (increment (lambda (label)
                        (let ((count (hash-ref counts label 0)))
                          (hash-set! counts label (+ count 1))))))
      (values
       (concurrent-or
        (timed-let loop ((count 0))
                   (increment 'a)
                   (if (>= count 4)
                       #f
                       (loop (+ count 1))))
        (timed-let loop ((count 0))
                   (increment '∞)
                   (loop (+ count 1)))
        (timed-let loop ((count 0))
                   (increment 'b)
                   (if (>= count 9)
                       'b
                       (loop (+ count 1)))))
       counts)))
  (lambda (result counts)
    (test-case
     "final result"
     (check eq? result 'b))
    (test-case
     "short finite loo["
     (let ((count (hash-ref counts 'a 0)))
       (check = count 5)))
    (test-case
     "long finite loop"
     (let ((count (hash-ref counts 'b 0)))
       (check = count 10)))
    (test-case
     "infinite loop"
     (let ((count (hash-ref counts '∞ 0)))
       (check = count 10))))))