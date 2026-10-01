#lang racket

;;; Copyright (c) Kirk Rader 2026

(require rackunit
         "timer.rkt"
         "engines.rkt"
         "concurrent-or.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; unit test for engines, concurrent-or, and timers

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
   (check eq? result 'b (format "expected result 'b, got '~a" result))
   (let ((count (hash-ref counts 'a)))
     (check = count 5 (format "expected 'a count to be 5, got ~a" count)))
   (let ((count (hash-ref counts 'b)))
     (check = count 10 (format "expected 'b count to be 10, got ~a" count)))
   (let ((count (hash-ref counts '∞)))
     (check = count 10 (format "expected '∞ count to be 10, got ~a" count)))))