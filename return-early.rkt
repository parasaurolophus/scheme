; Copyright (c) 2024-2026 Kirk Rader

#lang racket

; return-early demonstrates a basic use for continuations: implement the "return" statement common in
; many other languages
(define (return-early)

  (call-with-current-continuation

    (lambda (return)

      ; return is bound to call-with-current-continuation's continuation, which is in tail position
      ; relative to return-early

      (display 1)
      (newline)

      ; invoking return causes return-early's continuation to immediately receive 2 as its value
      (return 2)

      ; execution never reaches here because of the invocation of the return continuation in the
      ; preceding line
      (display 3)
      (newline))))

(module+ test

  (require rackunit)

  (let ((result (return-early)))
    (check = result 2 (format "expected 2, got ~a" result))))