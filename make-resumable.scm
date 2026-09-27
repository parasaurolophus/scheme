;; Copyright (c) Kirk Rader 2024-2026

;; make-resumable demonstrates using a continuation to resume a previously
;; exited flow-of-control multiple times

;; provide call/cc for strict r5rs or earlier implementation which lack it
(define-syntax call/cc
  (syntax-rules ()
    ((_ proc)
     (call-with-current-continuation proc))))

;; return a function which, when invoked, returns a continuation
;;
;; the resumable function's closure includes a counter which is initialized to 0
;; when it first invoked
;;
;; each time the resumable function's continuation is invoked, it increments the
;; counter and returns the updated value
;;
;; as a side-effect, the state of the counter and the value passed back into the
;; resumable function as a parameter to the continuation are logged to the
;; console
(define (make-resumable)
  (let ((counter 0))
    (lambda ()
      (call/cc
       (lambda (return)

         (display "initial value of counter is ")
         (display counter)
         (newline)

         ;; invoking return causes return-early's continuation to
         ;; immediately receive the resume continuation as its value
         (let ((resumed (call/cc (lambda (resume) (return resume)))))

           ;; execution only reaches here if the resume continuation is
           ;; invoked in which case resumed is bound to whatever was
           ;; passed to it

           (set! counter (+ counter 1))
           (display "resumed with ")
           (display resumed)
           (display ", counter is now ")
           (display counter)
           (newline)

           ;; return counter as the "final" value of return-resume
           counter))))))

;;; unit test for make-resumable
(define (test)

  ; bind c and k outside of the contination of the binding of r
  (let ((c #f)
        (k #f))

    ; set c to the continuation of a resumable computation by invoking the
    ; function returned from a call to make-resumable
    (let ((r (make-resumable)))
      (set! c (r)))

    (cond

      ((procedure? c)
       ; initial invocation of the resumable function returned a
       ; continuation; set k and invoke it for the first time
       (set! k c)
       (k 'foo))

      ((= c 1)
       ; k was invoked once, invoke it a second time
       (k 'bar))

      ((= c 2)
       ; k was invoked twice, invoke it a third time
       (k 'baz))

      ((= c 3)
       ; third time's the charm!
       (display "final value of c is ")
       (display c)
       (newline)
       c)

      (else
       (display "error! unexpected value for c ")
       (display c)
       (newline)))))

; invoking unit test
(test)
