;; Copyright 2024 Kirk Rader

;; naive implementation of n!, susceptible to stack-overflow
;;
;; this is a literal implementation of the following traditional
;; mathematical formula in scheme:
;;
;;      +- 1,         if n <= 1
;; n! = |
;;      +- n(n-1)!,   otherwise
;;
;; e.g. 4! = 4*3! = 4*3*2! = 4*3*2*1! = 4*3*2*1 = 24
;;
;; this implementation is mathematically correct, but the range of n
;; is limited by the amount of memory available in the call stack at
;; runtime
(define (factorial1 n)
  (if (<= n 1)
      1
      (* n (factorial1 (- n 1)))))
