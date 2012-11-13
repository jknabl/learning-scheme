#lang racket
;session 12 - concurrency

;concurrency: programs executing in parallel.
;benefit: increase in efficiency
;-order of execution matters a _lot_ when we have assignment

(define (f)
  (display "this will run in another thread"))

(thread f)

(define (parallel-execute . procs)
  (map thread procs))

;the following can execute to 5 different values:
(define x 10)
(parallel-execute
 (lambda () (set! x (* x x)))
 (lambda () (set! x (+ x 1))))
x

;to control the mechanisms by which we deem orderings acceptable, we can SERIALIZE access
;to shared state.
;
;-With serialization processes will execute concurrently, caveat there will be certain results 
;  that are not allowed concurrent execution.
;-Serialization creates distinct sets of procedures such that only one execution is allowed per
;  set.
;-If a procedure in the set is being executed, any other procedure in the same set
;  will have to wait to execute.
;
;A serializer takes a procedure as its argument and returns a serialized procedure
;that behaves like original procedure. 
;
; Example: bank account

;(define y 10)
;(define s (make-serializer))
;(parallel-execute
; (s (lambda () (set! y (* y y))))
; (s (lambda () (set! y (+ y 1)))))

; the above functions are serialized and won't run at the same time

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds"))

  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)

  (let ((s (make-serializer)))
    (define (dispatch m)
      (cond ((eq? m 'withdraw) (s withdraw))
            ((eq? m 'deposit) (s deposit))
            ((eq? m 'balance) balance) ;no problem if multiple ppl view balance @ same time
            (else
             (error "Request doesn't exist." m))))
    dispatch))

; The above will disallow simultaneous withdrawing/depositing from different processes
;
; Implement serializers using a MUTEX
; -Mutex supports two ops:
;   -Can be AQUIRED
;   -Can be RELEASED
; -Once a mutex is acquired, no other acquire ops can happen
;   until the mutex is released.

(define (make-serializer)
  (let ((mutex (make-mutex)))
    (lambda (p)
      (define (serialized-p . args)
        (mutex 'acquire)
        (let ((val (apply p args)))
          (mutex 'release)
          val))
      serialized-p)))

(define (make-mutex)
  (let ((cell #f))
    (define (the-mutex m)
      (cond ((eq? m 'acquire)
             (if (test-and-set-cell!)
                 (the-mutex 'acquire) ; retry
                 (display "mutex acquired")))
            ((eq? m 'release) (set! cell #f))))
    (define (test-and-set-cell!)
      (if cell
          #t
          (begin (set! cell #t) #f)))
    the-mutex
    ))
    ;atomically set cell value to true and return prev value