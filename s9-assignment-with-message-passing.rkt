#lang racket
;session 9 - assignment and state

;state requires an assignment operator

;use the example of a bank account. withdraw from it.

(define balance 100)
(define (withdraw amount)
  (if (>= balance amount)
      (begin
        (set! balance (- balance amount))
        balance)
      "Insufficient funds."))

"Test bank withdrawals. Starting balance is 100"
(withdraw 25)
(withdraw 25)
(withdraw 60)
(withdraw 15)
"reset balance to 100"
(set! balance 100)
balance

;this uses set! form:
;  (set! <name> <new-value> )

;begin form causes multiple expressions to be evaluated in place:
; (begin <exp1> <exp2> ... <expn>)
; value of <expn> is returned as value of entire begin form

;Note that the balance variable is GLOBAL.
;To make it LOCAL, we can use let form:

(define new-withdraw
  (let ((balance 100))
    (lambda (amount)
      (if (>= balance amount)
          (begin
            (set! balance (- balance amount))
            balance)
          "Insufficient funds."))))

(define x (new-withdraw 25))
x

; add the balance parameter to specify initial amount in account:

(define (make-withdraw balance)
  (lambda (amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds.")))

;now use make-withdraw to create independent accounts:

(define britta-perry (make-withdraw 100))
(define donald-trump (make-withdraw 100000))
(britta-perry 50)
(donald-trump 50000)
(britta-perry 75)
(donald-trump 75)
" - - - ------- - - - - - - - "
; now we can use message passing to make account objects that handle both
; deposits and withdrawals:

(define (make-account balance)
  (define (withdraw amount)
    (if (>= balance amount)
        (begin
          (set! balance (- balance amount))
          balance)
        "Insufficient funds."))
  (define (deposit amount)
    (set! balance (+ balance amount))
    balance)
  (define (dispatch m)
    (cond ((eq? m 'withdraw) withdraw)
          ((eq? m 'deposit) deposit)
          (else
           (error "Unknown account operation" m))))
  dispatch)

(define abed (make-account 200))
(define mitt-romney (make-account 500000))
((abed 'withdraw) 50)
((mitt-romney 'withdraw) 75000)
((abed 'deposit) 75)
((mitt-romney 'deposit) 500000)