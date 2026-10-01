;;; Copyright (c) Kirk Rader 2026

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Adapted to racket from Dybvig & Hieb's "Engines from Continuations" [1988]
;
; Note that the engines used here are defined by ./engines.rkt and should not be confused with those
; provide by the racket/engine package.

(require "timer.rkt" "engines.rkt"
         (for-syntax "timer.rkt"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Wrap each thunk in an engine and run them all in a round-robin queue. Terminate the queue when one
; of the engines returns a value other than #f or all engines complete. Note that this will work
; correctly even if one or more engines would never complete, so long as at least returns a value
; other than #f.
(define (first-true . thunks)
  (letrec ((engines '())
           (enqueue (lambda (engine)
                      (set! engines (reverse (cons engine (reverse engines))))))
           (dequeue (lambda ()
                      (if (pair? engines)
                          (let ((engine (car engines)))
                            (set! engines (cdr engines))
                            engine)
                          (error 'dequeue "queue is empty"))))
           (run (lambda ()
                  (and (pair? engines)
                       ((dequeue)
                        1
                        (lambda (result _ticks) (or result (run)))
                        (lambda (engine) (enqueue engine) (run)))))))
    (for-each (lambda (thunk)
                (enqueue (make-simple-engine thunk)))
              thunks)
    (run)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Co-operative multi-tasking version of "or" that uses first-true (q.v.)
(define-syntax concurrent-or
  (syntax-rules ()
    ((_)
     #f)
    ((_ expression ...)
     (first-true (timed-lambda () expression) ...))))

(provide first-true concurrent-or)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; unit test covers concurrent-or and, therefore, first-true, make-engine,  decrement-timer etc.
;
; Invoke from the command line `raco test concurrent-or.rkt`
(module+ test

  (require rackunit)

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
        (check = count 10 (format "expected '∞ count to be 10, got ~a" count))))))