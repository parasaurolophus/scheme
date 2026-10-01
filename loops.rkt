; Copyright (c) Kirk Rader 2026

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Convenience wrappers for simple loops

(require "timer.rkt"
         (for-syntax "timer.rkt"))

(define-syntax while
  (syntax-rules ()
    ((_ expression)
     (let ((test (lambda () expression)))
       (let loop ((continue (test)))
         (when continue
           (loop (test))))))
    ((_ expression body ...)
     (let ((test (lambda () expression)))
       (let loop ((continue (test)))
         (when continue
           body
           ...
           (loop (test))))))))

(define-syntax until
  (syntax-rules ()
    ((_ expression)
     (let ((test (lambda () expression)))
       (let loop ((continue (test)))
         (unless continue
           (loop (test))))))
    ((_ expression body ...)
     (let ((test (lambda () expression)))
       (let loop ((continue (test)))
         (unless continue
           body
           ...
           (loop (test))))))))

(define-syntax timed-while
  (syntax-rules ()
    ((_ expression)
     (while expression (decrement-timer)))
    ((_ expression body ...)
     (while expression (decrement-timer) body ...))))

(define-syntax timed-until
  (syntax-rules ()
    ((_ expression)
     (until expression (decrement-timer)))
    ((_ expression body ...)
     (until expression (decrement-timer) body ...))))

(provide while until timed-while timed-until)

(module+ test

  (require rackunit)

  (let ((count 0))
    (while (< count 3)
           (set! count (+ count 1)))
    (check = count 3 (format "'while' expected 3, got ~a~%" count)))

  (let ((count 0))
    (until (> count 2)
           (set! count (+ count 1)))
    (check = count 3 (format "'until' expected 3, got ~a~%" count))))