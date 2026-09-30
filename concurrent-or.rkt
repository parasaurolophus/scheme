#lang racket

;;; Copyright (c) Kirk Rader 2026

(require "timer.rkt"
         "engines.rkt")

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
                        (lambda (result ticks) (or result (run)))
                        (lambda (engine) (enqueue engine) (run)))))))
    (for-each (lambda (thunk)
                (enqueue (make-simple-engine thunk)))
              thunks)
    (run)))

(define-syntax concurrent-or
  (syntax-rules ()
    ((_ expression ...)
     (first-true (timed-lambda () expression) ...))))

(provide first-true
         concurrent-or)