;; Copyright 2016-2024 Kirk Rader

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
;;             (printf "engine returned ~a with ~a remaining ticks~%" value remaining-ticks))
;;
;;          ;; the "expiration" routine
;;          (lambda (new-engine)
;;             (printf "engine interrupted; use the given new engine to resume~%"))))

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
            (start-timer! (call/cc do-expire) timer-handler)))
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
              ((call/cc
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
          (call/cc (lambda (resume) (do-expire)))))

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
