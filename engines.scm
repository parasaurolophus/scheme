#lang racket

;; Copyright 2016-2026 Kirk Rader

;; Adapted from Dybvig and Hieb, "Engines from Continuations" [1988]

;; "Engines" are an abstraction for decomposing a program into
;; procedures that can be scheduled to run for only a limited amount
;; of time. If an engine runs out of "fuel" before it has completed,
;; it can later be resumed from the point at which it was
;; interrupted. Using this simple mechanism it is trivial to implement
;; co-routines, a light-weight round-robin task scheduler etc.

;; In particular, an engine is a procedure of three arguments, created
;; from a procedure of zero argments. E.g.

;;    (let ((my-engine (make-engine (lambda ()
;;                                     (decrement-timer!)
;;                                     (display "engine running")
;;                                     (newline)))))
;;       (my-engine
;;
;;          ;; the amount of "fuel" for this engine,
;;          ;; i.e. the number of times decrement-timer!
;;          ;; can be called before this engine expires
;;          1
;;
;;          ;; the "completion" routine
;;          (lambda (value remaining-ticks)
;;             (display "engine returned ")
;;             (display value)
;;             (display " with ")
;;             (display remaining-ticks)
;;             (display "remaining ticks")
;;             (newline))
;;
;;          ;; the "expiration" routine
;;          (lambda (new-engine)
;;             (display "engine interrupted")
;;             (display "; use the given new engine to resume")
;;             (newline))))

;; When invoked, the engine will call the procedure from which it was
;; created. In addition, the engine will interrupt the procedure after
;; the number of "ticks" specified by its first argument have elapsed.
;; If the engine's procedure returns before the given number of ticks
;; have elapsed, it invokes the procedure passed as its second
;; argument. If the engine's procedure is interrupted before
;; returning, the engine invokes the procedure passed as its third
;; argument. The former is passed the value returned by the engine's
;; procedure and the remaining number of unconsumed ticks. The latter
;; is passed a new engine that is the continuation of the one that was
;; interrupted.

;; NOTE WELL!!
;; ===========

;; This implementation of engines does not rely on true threads as are
;; available in many modern Scheme implementations (e.g. Guile). Nor
;; does it assume any mechanism for overriding the built-in lambda,
;; let, letrec and similar special forms with versions that implicitly
;; invoke decrement-timer! (q.v.), as was assumed in Dybvig and Hieb's
;; original papaer. This means that you *must* call decrement-timer!
;; explicitly at strategic points in your engine procedures or else
;; they will never yield to other engines.

;; Many modern Scheme implementations offer "apply hooks" and similar
;; mechanisms for injecting custom function calls down in the guts of
;; Scheme's run time machinery. You can use such mechanisms to arrange
;; to have decrement-timer! called implicitly, but you may get
;; surprising results depending on the particular mechanism you
;; use. For example, a simple "apply hook" is likely not to be invoked
;; at exactly the right execution points for code that uses primarily
;; built-in special forms for which the compiler does not actually
;; emit function calls. Conversely, for code with many calls to actual
;; functions, decrementing the timer at each and every application not
;; only adds significant overhead but also means that you might need
;; to carefully tune the number of ticks you pass to each engine to
;; give each one a reasonably fair chance to run.

;; For these reasons, it is actually far more reliable simply to
;; follow the policy assumed by this implementation and put the onus
;; on the programmer to call decrement-timer! explicitly at sensible
;; "synchronization points" in engines. This does mean that an engine
;; is really just an elaborate version of "apply" if the programmer
;; fails to call decrement-timer! often enough in inner loops. Note
;; that this is comparable to goroutines in Go, or similar cooperative
;; multi-tasking constructs in other languages, where concurrency is
;; only effective when all of the tasks make the kinds of calls
;; necessary to hand off control to one another at an appropriate
;; rate.

(define decrement-timer! #f)
(define make-engine #f)
(define engine-block #f)
(define engine-return #f)

(letrec ((active? #f)
         (do-return #f)
         (do-expire #f)
         (clock 0)
         (handler '())
         (timer-handler
          (lambda ()
            (start-timer!
             (call-with-current-continuation do-expire)
             timer-handler)))
         (start-timer!
          (lambda (ticks new-handler)
            (set! handler new-handler)
            (set! clock ticks)))
         (stop-timer!
          (lambda ()
            (let ((remaining clock))
              (set! clock 0)
              (set! handler '())
              remaining)))
         (new-engine
          (lambda (resume)
            (lambda (ticks return expire)
              (if active?
                  (error 'engine "attempt to nest engines")
                  (set! active? #t))
              ((call-with-current-continuation
                (lambda (escape)
                  (set! do-return
                        (lambda (value ticks)
                          (set! active? #f)
                          (escape (lambda () (return value ticks)))))
                  (set! do-expire
                        (lambda (resume)
                          (set! active? #f)
                          (escape (lambda () (expire (new-engine resume))))))
                  (resume ticks))))))))

  ;; Decrement the timer.
  ;;
  ;; Invokes the current handler if the timer expires as a result of
  ;; this call.
  ;;
  ;; Usage: (decrement-timer!)
  ;;
  ;; See: start-timer!, stop-timer!
  (set! decrement-timer!
        (lambda ()
          (when (> clock 0)
            (set! clock (- clock 1))
            (when (< clock 1)
              (let ((h handler))
                (stop-timer!)
                (h))))))

  ;; Function: (make-engine thunk)
  ;;
  ;; Creates an engine from the given procedure.
  ;;
  ;; thunk - a zero-argument procedure
  ;;
  ;; Usage:
  ;;
  ;;    (let ((my-engine (make-engine (lambda ()
  ;;                                     (decrement-timer!)
  ;;                                     (display "engine running")
  ;;                                     (newline)))))
  ;;       (my-engine
  ;;
  ;;          ;; the amount of "fuel" for this engine,
  ;;          ;; i.e. the number of times decruement-timer!
  ;;          ;; can be called before this engine expires
  ;;          1
  ;;
  ;;          ;; the "completion" routine
  ;;          (lambda (value remaining-ticks)
  ;;             (printf "engine returned ~a with ~a remaining ticks~%" value remaining-ticks))
  ;;
  ;;          ;; the "expiration" routine
  ;;          (lambda (new-engine)
  ;;             (printf "engine interrupted; use the given new engine to resume~%"))))
  ;;
  ;; See: engine-block, engine-return
  (set! make-engine
        (lambda (thunk)
          (new-engine
           (lambda (ticks)
             (start-timer! ticks timer-handler)
             (engine-return (thunk))))))

  ;; Function: (engine-block)
  ;;
  ;; Cause the currently executing engine to immediately yield to its
  ;; expiration handler as if its fuel had run out.
  ;;
  ;; See: make-engine, engine-return
  (set! engine-block
        (lambda ()
          (call-with-current-continuation
           (lambda (resume)
             (do-expire)))))

  ;; Function: (engine-return value)
  ;;
  ;; Return immediately from the currently executing engine.
  ;;
  ;; This will cause the currently active engine to immediately exit and
  ;; so for its completion handler to be invoked.
  ;;
  ;; value - The value to return as the result of the engine's computation.
  ;;
  ;; Usage: (engine-return 'my-value)
  ;;
  ;; See: make-engine, engine-block
  (set! engine-return
        (lambda (value)
          (if active?
              (let ((ticks (stop-timer!)))
                (do-return value ticks))
              (error 'engine "no engine running")))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; Tests

(define first-true #f)

;; Macro: (concurrent-or ...)
;;
;; Like (or ...) except that it will return if even one expression
;; returns a value other than #f, even if one or more expressions
;; never return.
;;
;; See: first-true
(define-syntax concurrent-or
  (syntax-rules ()
    ((_ e ...)
     (first-true (lambda () e) ...))))

;; Unit test for engines.
;;
;; Invoke concurrent-or on two expressions, the first of which never
;; returns.
;;
;; count - the argument to pass to finite-loop (q.v.)
(define (engines-test count)
  (letrec ((infinite-loop-count 0)
           (finite-loop-count 0)
           (infinite-loop
            (lambda ()
              (decrement-timer!)
              (set! infinite-loop-count
                    (+ infinite-loop-count 1))
              (display "infinite loop")
              (newline)
              (infinite-loop)))
           (finite-loop
            (lambda (count)
              (decrement-timer!)
              (set! finite-loop-count
                    (+ finite-loop-count 1))
              (if (> count 0)
                  (begin
                    (display "finite loop count ")
                    (display count)
                    (newline)
                    (finite-loop (- count 1)))
                  (begin
                    (display "infinite-loop-count ")
                    (display infinite-loop-count)
                    (newline)
                    (display "finite-loop-count ")
                    (display finite-loop-count)
                    (newline)
                    #t)))))
    (concurrent-or
     (infinite-loop)
     (finite-loop count))))

(let ((make-queue
       (lambda ()
         (let ((front '())
               (back '()))

           (lambda (message . arguments)

             (case message

               ((enqueue)
                (when (null? arguments)
                  (error 'queue "missing argument to push"))
                (set! back (cons (car arguments) back)))

               ((dequeue)
                (when (null? front)
                  (set! front (reverse back))
                  (set! back '()))
                (when (null? front)
                  (error 'queue "empty queue"))
                (let ((value (car front)))
                  (set! front (cdr front))
                  value))

               ((empty?)
                (and (null? front) (null? back)))

               (else
                (error 'queue "unsupported message" message))))))))

  ;; Function: (first-true . thunks)
  ;;
  ;; Return the value of the first of the given procedures to return a
  ;; value other than #f or #f if all of the procedures terminate with
  ;; the value #f.
  ;;
  ;; Uses engines to interleave execution of the given procedures such
  ;; that first-true will return if at least one of the procedures
  ;; returns a value other then #f even if one or more of the procedures
  ;; never returns.
  ;;
  ;; As with make-engine and engine-return, this is adapted from Dybvig
  ;; and Hieb, "Engines from Continuations" [1988]. It serves as a unit
  ;; test for engines. Note that this demonstrates the power of engines
  ;; to implement an extremely light-weight co-operative multi-tasking
  ;; mechanism in pure Scheme.
  ;;
  ;; See: make-engine, concurrent-or
  (set! first-true
        (lambda thunks
          (letrec ((engines
                    ;; FIFO queue of engines to run
                    (make-queue))
                   (run
                    ;; execute each engine in the queue, removing engines that
                    ;; terminate, re-enqueueing ones that expire, until one
                    ;; returns a value other than #f
                    (lambda ()
                      (if (engines 'empty?)
                          #f
                          (let ((engine (engines 'dequeue)))
                            (engine
                             1
                             (lambda (result ticks) (or result (run)))
                             (lambda (engine) (engines 'enqueue engine) (run))))))))
            (for-each (lambda (thunk)
                        (engines 'enqueue (make-engine thunk)))
                      thunks)
            (run)))))

(engines-test 10)