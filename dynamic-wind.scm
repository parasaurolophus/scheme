;; Copyright (c) 2024 Kirk Rader

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; wrap a continuation created using ./return-resumable.scm in stack
;; winding / unwinding protection
(define (continuation-demo)

  (let ((resumable (return-resumable)))

    (dynamic-wind

      ;; the "before" thunk is invoked each time execution enters the
      ;; protected dynamic context
      (lambda () (printf "~%entering protected context~%"))

      ;; the "body" thunk is executed after the "before" thunk has
      ;; returned and before the "after" thunk is invoked, each time
      ;; execution enters or leaves the body of this call to
      ;; dynamic-wind
      resumable

      ;; the "after" thunk is invoked each time execution leaves the
      ;; protected dynamic context
      (lambda () (printf "exiting protected context~%")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; put all of the preceding togehter; this will write the following to
;; stdout:
;;
;;   entering protected context
;;   count is 0
;;   exiting protected context
;;
;;   entering protected context
;;   resumed with foo, count is now 1
;;   exiting protected context
;;
;;   entering protected context
;;   resumed with bar, count is now 2
;;   exiting protected context
;;
;;   entering protected context
;;   resumed with baz, count is now 3
;;   exiting protected context
;;   3
(define (test)

  ;; bind k outside of the continuation of the definition of c
  (let ((k #f))

    ;; bind c to the value returned by invoking (continuation-demo);
    ;; i.e. c will initially be bound to the continuation named resume
    ;; in the body of return-resumable
    (let ((c (continuation-demo)))

      ;; execution will enter the body of this let multiple times
      ;; despite the lack of an explicit looping construct since
      ;; invocation of an inner continuation within the body of its
      ;; outer continuation amounts to self-recursion

      (cond ((procedure? c)
             (set! k c)
             (k 'foo))

            ((= c 1)
             (k 'bar))

            ((= c 2)
             (k 'baz))

            (else
             ;; the final result returned by (test) is 3
             c)))))
