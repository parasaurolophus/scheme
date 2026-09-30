#lang racket

;;; Copyright (c) Kirk Rader 2026

;;; Adapted to racket from Appendix A of Dybvig & Hieb,
;;; "Engines from Continuations" [1988]

;;; This is the most general-purpose version of nestable engines presented by
;;; Dybvig & Hieb.

;;; No attempt is made here to provide overloaded versions of lambda or other
;;; special forms that would implicitly decrement the timer. This means that
;;; you must explicitly call "decrement-timer" in the body of your engine

(define start-timer #f)
(define stop-timer #f)
(define decrement-timer #f)
(define make-engine #f)
(define make-simple-engine #f)

(letrec ((clock 0)
         (handler #f)
         (simplify (lambda (engine)
                     (lambda (ticks return expire)
                       (engine
                        ticks
                        (lambda (value ticks engine-maker)
                          (return value ticks))
                        (lambda (engine)
                          (expire (simplify engine)))))))
         (new-engine (lambda (proc id)
                       (lambda (ticks return expire)
                         ((call/cc (lambda (k)
                                     (run
                                      proc
                                      (stop-timer)
                                      ticks
                                      (lambda (value ticks engine-maker)
                                        (k (lambda ()
                                             (return
                                              value
                                              ticks
                                              engine-maker))))
                                      (lambda (engine)
                                        (k (lambda ()
                                             (expire engine))))
                                      id)))))))
         (run (lambda (resume parent child return expire id)
                (let ((ticks (if (and (active?) (< parent child))
                                 parent
                                 child)))
                  (push (- parent ticks) (- child ticks) return expire id)
                  (resume ticks))))
         (go (lambda (ticks)
               (when (active?)
                 (if (= ticks 0)
                     (timer-handler)
                     (start-timer ticks timer-handler)))))
         (do-return (lambda (proc value ticks id1)
                      (pop (lambda (parent child return expire id2)
                             (if (eq? id1 id2)
                                 (begin
                                   (go (+ parent ticks))
                                   (return
                                    value
                                    (+ child ticks)
                                    (lambda (value)
                                      (new-engine (proc value) id1))))
                                 (do-return
                                  (lambda (value)
                                    (lambda (new-ticks)
                                      (run
                                       (proc value)
                                       new-ticks
                                       (+ child ticks)
                                       return
                                       expire
                                       id2)))
                                  value
                                  (+ parent ticks)
                                  id1))))))
         (do-expire (lambda (resume)
                      (pop (lambda (parent child return expire id)
                             (if (> child 0)
                                 (do-expire (lambda (ticks)
                                              (run
                                               resume
                                               ticks
                                               child
                                               return
                                               expire
                                               id)))
                                 (begin
                                   (go parent)
                                   (expire (new-engine resume id))))))))
         (timer-handler (lambda () (go (call/cc do-expire))))
         (stack '())
         (push (lambda l (set! stack (cons l stack))))
         (pop (lambda (handler)
                (if (null? stack)
                    (error 'engine "attempt to return from inactive engine")
                    (let ((top (car stack)))
                      (set! stack (cdr stack))
                      (apply handler top)))))
         (active? (lambda () (pair? stack))))
  (set! start-timer (lambda (ticks new-handler)
                      (set! handler new-handler)
                      (set! clock ticks)))
  (set! stop-timer (lambda ()
                     (let ((remaining clock))
                       (set! clock 0)
                       remaining)))
  (set! decrement-timer
        (lambda ()
          (when (> clock 0)
            (set! clock (- clock 1))
            (when (= clock 0) (handler)))))
  (set! make-engine (lambda (proc)
                      (letrec ((engine-return
                                (lambda (value)
                                  (call/cc
                                   (lambda (k)
                                     (do-return
                                      (lambda (value)
                                        (lambda (ticks)
                                          (go ticks)
                                          (k value)))
                                      value
                                      (stop-timer)
                                      engine-return))))))
                        (new-engine (lambda (ticks)
                                      (go ticks)
                                      (proc engine-return)
                                      (error 'engine "invalid completion"))
                                    engine-return))))
  (set! make-simple-engine (lambda (proc)
                             (simplify (make-engine (lambda (engine-return)
                                                      (engine-return (proc))))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Define concurrent-or using engines

(define (first-true . thunks)
  (letrec ((engines '())
           (enqueue (lambda (engine)
                      (set! engines (reverse (cons engine (reverse engines))))))
           (dequeue (lambda ()
                      (if (pair? engines)
                          (let ((engine (car engines)))
                            (set! engines (cdr engines))
                            engine)
                          (error 'dequeue "queue is empty"))))
           (run (lambda ()
                  (and (pair? engines)
                       ((dequeue)
                        1
                        (lambda (result ticks) (or result (run)))
                        (lambda (engine) (enqueue engine) (run)))))))
    (for-each (lambda (thunk)
                (enqueue (make-simple-engine thunk)))
              thunks)
    (run)))

(define-syntax concurrent-or
  (syntax-rules ()
    ((_ expression ...)
     (first-true (lambda () expression) ...))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(let ((infinite-loop (lambda (label)
                       (lambda ()
                         (let loop ((count 0))
                           (decrement-timer)
                           (printf "~a ~a~%" label count)
                           (loop (+ count 1))))))
      (finite-loop (lambda (label max result)
                     (lambda ()
                       (let loop ((count 0))
                         (decrement-timer)
                         (printf "~a ~a~%" label count)
                         (if (>= count max)
                             result
                             (loop (+ count 1))))))))
  (concurrent-or
   ((finite-loop 'finite-a 4 #f))
   ((infinite-loop 'infinite))
   ((finite-loop 'finite-b 9 'b))))