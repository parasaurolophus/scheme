;; Copyright 2024 Kirk Rader

;; idiomatic implementation of the same logic as in factorial3.scm
;;
;; this version implements the helper function, f, using a named, thus
;; acheiving a simpler syntax that looks fairly close to the original,
;; non-tail-recursive version in factorial1.scm while still
;; benefitting from tail-call optimization
(define (factorial4 n)
  (let f ((x n)
          (a 1))
    (if (<= x 1)
        a
        (f (- x 1) (* a x)))))
