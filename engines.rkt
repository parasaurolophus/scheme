;;; Copyright (c) Kirk Rader 2026

#lang racket

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
; Adapted to racket from Appendix A of Dybvig & Hieb, "Engines from Continuations" [1988]
;
; This is the most general-purpose version of engines presented by Dybvig & Hieb in their origianl
; paper on the subject of engines. It supports nesting engines (i.e. engines that are created and run
; within the bodies of engine procedures) and dynamically choosing which engine to resume when a given
; engine terminates. The latter supports complex flows-of-control for tasks such as breadth-first
; search.
;
; No attempt is made here to provide overloaded versions of lambda or other special forms that would
; implicitly decrement the timer. This means that you must explicitly call "decrement-timer" in the
; bodies of your engine procedures. See ./timer.rkt and ./loops.rkt for convenience macros like
; timed-lambda, timed-let, timed-while, etc.
;
; Note that this version of engines should not be confused with those provided by the racket/engine
; package. For production purposes, racket's thread-based engines are greatly to be preferred. This
; version is presented for historical and tutorial purposes. In particular, this version demonstrates
; that Scheme's first-class continuations together with tail-call optimization is sufficiently
; powerful to implement any kind of flow-of-control, including cooperative multitasking of the sort
; that was standard in microcomputer operating systems of the 1980's and 1990's, and is still in
; extremely wide-spread use in browser-based web applications and platforms like node.js (an
; asynchronous function in javascript that relies on promises is an example of modern-day cooperative
; multitasking).

(require "timer.rkt")

(define make-engine #f)
(define make-simple-engine #f)

(letrec ((simplify (lambda (engine)
                     ; helper used by make-simple-engine
                     (lambda (ticks return expire)
                       (engine
                         ticks
                         (lambda (value ticks _engine-maker)
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

(provide make-engine make-simple-engine)