#lang racket

;; Copyright (c) 2024-2026 Kirk Rader

;; return-resume demonstrates using a continuation to resume a
;; previously exited flow-of-control
(define (return-resume)

  (call-with-current-continuation
   
   (lambda (return)

     ;; return is bound call-with-current-continuation continuation, which is in
     ;; tail position relative to return-resume

     (display 1)
     (newline)

     ;; invoking return causes return-early's continuation to immediately
     ;; receive the resume continuation as its value

     (let ((resumed (call-with-current-continuation
                     (lambda (resume)
                       (return resume)))))

       ;; execution only reaches here if the resume continuation is
       ;; invoked in which case resumed is bound to whatever was
       ;; passed to it

       (display resumed)
       (newline)

       ;; return 2 as the final value of return-resume
       2))))

(let ((k (return-resume)))
  (cond
    ((procedure? k) (k 'foo))
    ((= k 2) 'success)
    (else (error 'return-resume k))))