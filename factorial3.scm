;; Copyright 2024 Kirk Rader

;; tail-recursive, if a bit verbose, implementation of n! that is not
;; susceptible to stack-overflow
;;
;; here is a mathematically equivalent variation of the traditional
;; formula but using a (still self-referential) helper function:
;;
;; n! = f(n, 1)
;;                 +- a,            if x <= 1
;; where f(x, a) = |
;;                 +- f(x-1, ax),   otherwise
;;
;; while f explicitly implements the same self-recursive logic as
;; factorial1, it does so by making the recursive call in
;; tail-position, which in scheme makes all the difference. the scheme
;; specification requires that the compiler detect such tail-calls and
;; optimize them such that they perform as or more efficiently as
;; special-purpose looping constructs like "do" while not growing the
;; stack
;;
;; e.g. 4! = f(4,1) = f(3, 4*1) = f(2, 4*3*1) = f(1, 4*3*2*1) = 24
(define (factorial3 n)
  (letrec ((f (lambda (x a)
                (if (<= x 1)
                    a
                    (f (- x 1) (* a x))))))
    (f n 1)))
