; Copyright 2024 Kirk Rader

#lang racket

; idiomatic implementation of the same logic as in ./factorial3.rkt
;
; this version implements the helper function, f, using a named let, thus acheiving a simpler syntax
; that looks closer to the original, non-tail-recursive version in factorial1.rkt while still
; benefitting from tail-call optimization
(define (factorial4 n)
  (let f ((x n)
          (a 1))
    (if (<= x 1)
        a
        (f (- x 1) (* a x)))))

(module+ test

  (require rackunit)

  (let ((result (factorial4 5)))
    (check = result 120 (format "expected 120, got ~A" result))))