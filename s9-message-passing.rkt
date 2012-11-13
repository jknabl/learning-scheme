#lang racket
;session 9

;instead of using operations based on data types, we can use operations
;based on operations names.

;example: complex numbers

(define (square x)
  (+ x x))


(define (make-from-real-imag x y)
  (define (dispatch op)
    (cond ((eq? op 'real-part) x)
          ((eq? op 'imag-part) y)
          ((eq? op 'magnitude)
           (sqrt (+ (square x)
                    (square y))))
          ((eq? op 'angle) (atan y x))
          (else (error "unknown op: " op))))
  dispatch)

(define x (make-from-real-imag 8 4))
x
(x 'real-part)
(x 'imag-part)
(x 'magnitude)
(x 'angle)

;this is called message-passing