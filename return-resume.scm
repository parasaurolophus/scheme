;; Copyright (c) 2024 Kirk Rader

;; return-resume demonstrates using a continuation to resume a
;; previously exited flow-of-control
(define (return-resume)
  (call/cc
   (lambda (return)

     ;; return is bound call/cc's continuation, which is in tail
     ;; position relative to return-resume

     (displayln 1)

     ;; invoking return causes return-early's continuation to
     ;; immediately receive the resume continuation as its value
     (let ((resumed (call/cc (lambda (resume) (return resume)))))

       ;; execution only reaches here if the resume continuation is
       ;; invoked in which case resumed is bound to whatever was
       ;; passed to it
       (displayln resumed)

       ;; return 2 as the final value of return-resume
       (return 2)))))
