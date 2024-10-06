;; Copyright 2016-2024 Kirk Rader

;; Adapted from "Engines from Continuations," Dybvig and Hieb [1988]

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

