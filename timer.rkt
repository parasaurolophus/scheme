; Copyright (c) Kirk Rader 2026

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Adapted to racket from Dybvig & Hieb's "Engines from Continuations" [1988]

(define start-timer #f)
(define stop-timer #f)
(define decrement-timer #f)

(letrec ((clock 0)
         (handler #f))
  (set! start-timer (lambda (ticks new-handler)
                      (set! handler new-handler)
                      (set! clock ticks)))
  (set! stop-timer (lambda ()
                     (let ((remaining clock))
                       (set! clock 0)
                       remaining)))
  (set! decrement-timer
        (lambda ()
          (when (> clock 0)
            (set! clock (- clock 1))
            (and (= clock 0)
                 (procedure? handler)
                 (handler))))))

(define-syntax timed-lambda
  (syntax-rules ()
    ((_ variables form ...)
     (lambda variables (decrement-timer) form ...))))

(define-syntax (timed-let stx)
  (syntax-case stx ()
    ((_ bindings)
     #'(let bindings (decrement-timer)))
    ((_ bindings form)
     #'(let bindings (decrement-timer) form))
    ((_ first second form ...)
     (if (identifier? #'first)
         #'(let first second (decrement-timer) form ...)
         #'(let first (decrement-timer) second form ...)))))

(define-syntax timed-let*
  (syntax-rules ()
    ((_ bindings form ...)
     (let* bindings (decrement-timer) form ...))))

(define-syntax timed-letrec
  (syntax-rules ()
    ((_ bindings form ...)
     (letrec bindings (decrement-timer) form ...))))

(provide start-timer
         stop-timer
         decrement-timer
         timed-lambda
         timed-let
         timed-let*
         timed-letrec)

(module+ test

  (require rackunit)

  (let ((count 0))
    (call/cc
      (lambda (escape)
        (letrec ((handler (lambda () (escape count)))
                 (loop (lambda ()
                         (set! count (+ count 1))
                         (decrement-timer)
                         (loop))))
          (dynamic-wind
            (lambda () (start-timer 2 handler))
            loop
            stop-timer))))
    (check = 2 count (format "expected 2, got ~a" count)))

  (let ((count 0))
    (call/cc
      (lambda (escape)
        (letrec ((handler (lambda () (escape count)))
                 (loop (timed-lambda ()
                                     (set! count (+ count 1))
                                     (loop))))
          (dynamic-wind
            (lambda () (start-timer 2 handler))
            loop
            stop-timer))))
    (check = 1 count (format "expected 1, got ~a" count)))

  (let ((count 0))
    (call/cc
      (lambda (escape)
        (dynamic-wind
          (lambda () (start-timer 2 (lambda () (escape count))))
          (lambda ()
            (timed-let loop ()
                       (set! count (+ count 1))
                       (loop)))
          stop-timer)))
    (check = 1 count (format "expected 1, got ~a" count)))

  (let ((count 0))
    (call/cc
      (lambda (escape)
        (dynamic-wind
          (lambda () (start-timer 2 (lambda () (escape count))))
          (lambda ()
            (let loop ((_continue #t))
              (timed-let* ((increment (lambda () (set! count (+ count 1))))
                           (f (lambda () (increment))))
                          (f)
                          (loop #t))))
          stop-timer)))
    (check = 1 count (format "timed-let* expected 1, got ~a" count)))

  (let ((count 0))
    (call/cc
      (lambda (escape)
        (dynamic-wind
          (lambda () (start-timer 2 (lambda () (escape count))))
          (lambda ()
            (let loop ((_continue #t))
                   (timed-letrec ((increment (lambda () (set! count (+ count 1))))
                                  (f (lambda () (increment))))
                                 (f)
                                 (loop #t))))
          stop-timer)))
    (check = 1 count (format "timed-let* expected 1, got ~a" count))))