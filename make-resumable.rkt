#lang racket

;;; Copyright (c) Kirk Rader 2024-2026

(require rackunit)

;;; return a resumable function
;;;
;;; the function returned by calling `make-resumable` itself returns a
;;; continuation from within a closed-over environment containing a `counter`
;;; initializzed to to 0
;;;
;;; invoking the continuation increments `counter` and returns its new value
;;;
;;; the current state of `counter` and the value passed to the continuation are
;;; logged to the console
(define (make-resumable)

  (let ((counter 0))
    (lambda ()
      (call-with-current-continuation
       (lambda (return)
         (display "counter is initially ")
         (display counter)
         (newline)
         (let ((resumed (call-with-current-continuation
                         (lambda (k) (return k)))))
           (set! counter (+ counter 1))
           (display "resumed with ")
           (display resumed)
           (display ", counter is now ")
           (display counter)
           (newline)
           counter))))))

;;; unit test for `make-resumable`
(test-begin

 (let ((c #f)
       (k #f))

   (set! c ((make-resumable)))

   ; at this point, `c` is the continuation of the first invocation of a
   ; function returned by `make-resumable`

   ; since the invocation of `(set! c ...)` is the continuation of the
   ; contunuation returned by `((make-resumable))`, `c` will be updated again
   ; and the following `cond` invoked  each time the resumable function's
   ; continuation is called

   ; i.e. invoking a continuation always results in an implicit loop to some
   ; earlier point in a program's exeution if that continuation, itself, ever
   ; returns to its caller

   ; as famously demonstrated by Dybvig and Hieb, this implicit looping
   ; behavior can be exploited to implement bi-directional ommunication
   ; between co-routines whose execution is interleaved by passing and
   ; returning continuations

   ; [see <https://github.com/parasaurolophus/scheme/blob/main/engines.rkt> for
   ; more information]

   (cond

     ; when `c` is a continuation, save it to `k` and then invoke it for the
     ; first time
     ((procedure? c)
      (set! k c)
      (k 'first))

     ; after the first invocation of the resumable function, `c` will be
     ; updated to the current value of the resumable function's `counter` each
     ; time `k` is called as consequence of the continuation returning 

     ((= c 1) (k 'second))

     ((= c 2) (k 'third))

     ((= c 3) (check = c 3))

     ; execution should never reach here!
     (else (fail)))))

(provide make-resumable)