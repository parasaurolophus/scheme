;; Copyright 2024 Kirk Rader

;; don't do this!
;;
;; you can, of course, but expect pitying glances and unkind remarks
;; during code reviews by experienced scheme programmers even though
;; this is the only practical approach in most programming languages,
;; even other dialects of lisp
;;
;; this replaces the traditional, explicitly self-referential
;; definition of n! with a version where the self-reference is masked
;; behind a looping special form (which the scheme compiler will
;; transform under the covers into something much more like that in
;; factorial3.scm, by application of compile-time CPS optimizations)
(define (factorial2 n)
  (let ((a 1))
    (do ((x n (- x 1)))
        ((<= x 1) a)
      (set! a (* a x)))))
