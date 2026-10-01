;; Copyright (c) 2024 Kirk Rader

#lang racket

(require "make-resumable.rkt")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; wrap a continuation created using ./make-resumable.rkt in stack ; winding / unwinding protection
;
; This function will write the same output to the console as make-resumable, with additional lines
; interpolated due to the before and after thunks inject by dynamic-wind
(define (dynamic-wind-demo)

  (let ((resumable (make-resumable)))

    (dynamic-wind

      ;; the "before" thunk is invoked each time execution enters the ; protected dynamic context
      (lambda () (printf "~%entering protected context~%"))

      ;; the "body" thunk is executed after the "before" thunk has ; returned and before the "after"
      ;thunk is invoked, each time ; execution enters or leaves the body of this call to ;
      ;dynamic-wind
      resumable

      ;; the "after" thunk is invoked each time execution leaves the ; protected dynamic context
      (lambda () (printf "exiting protected context~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; bind k outside of the continuation of the definition of c
(let ((k #f))

  ; bind c to the value returned by invoking (continuation-demo); i.e. c will initially be bound to
  ; the continuation named resume in the body of make-resumable
  (let ((c (dynamic-wind-demo)))

    ; execution will enter the body of this let multiple times despite the lack of an explicit looping
    ; construct since invocation of an inner continuation within the body of its outer continuation
    ; amounts to self-recursion

    (cond ((procedure? c)
           (set! k c)
           (k 'foo))

          ((= c 1)
           (k 'bar))

          ((= c 2)
           (k 'baz))

          (else
            ; the final result is 3
            c))))