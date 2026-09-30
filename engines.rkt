#lang racket

;;; Copyright (c) Kirk Rader 2026

;;; Adapted to racket from Appendix A of Dybvig & Hieb,
;;; "Engines from Continuations" [1988]

;;; This is the most general-purpose version of nestable engines presented by
;;; Dybvig & Hieb.

;;; No attempt is made here to provide overloaded versions of lambda or other
;;; special forms that would implicitly decrement the timer. This means that
;;; you must explicitly call "decrement-timer" in the bodies of your engine
;;; procedures.

(require "timer.rkt")

(define make-engine #f)
(define make-simple-engine #f)

(letrec ((simplify (lambda (engine)
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

(provide make-engine
         make-simple-engine)