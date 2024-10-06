;; Copyright (c) 2024 Kirk Rader

;; return-resumable demonstrates using a continuation to resume a
;; previously exited flow-of-control multiple times
(define (return-resumable)
  (let ((counter 0))
    (lambda ()
      (call/cc
       (lambda (return)

         ;; return is bound call/cc's continuation, which is in tail
         ;; position relative to return-resume

         (printf "initial value of counter: ~a~%" counter)

         ;; invoking return causes return-early's continuation to
         ;; immediately receive the resume continuation as its value
         (let ((resumed (call/cc (lambda (resume) (return resume)))))

           ;; execution only reaches here if the resume continuation is
           ;; invoked in which case resumed is bound to whatever was
           ;; passed to it

           (set! counter (+ counter 1))
           (printf "resumed with ~a, counter is now ~a~%" resumed counter)

           ;; return counter as the "final" value of return-resume
           (return counter)))))))
