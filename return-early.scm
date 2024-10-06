;; Copyright (c) 2024 Kirk Rader

;; return-early demonstrates a basic use for continuations: implement
;; the "return" statement common in many other languages
(define (return-early)
  (call/cc
   (lambda (return)

     ;; return is bound to call/cc's continuation, which is in tail
     ;; position relative to return-early

     (displayln 1)

     ;; invoking return causes return-early's continuation to
     ;; immediately receive 2 as its value
     (return 2)

     ;; execution never reaches here because of the invocation of the
     ;; return continuation in the preceding line
     (displayln 3))))
