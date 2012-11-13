#lang racket
(require racket/stream)
(require racket/mpair)
;s13 - streams

;to avoid assignment, we can use a sequence to record values of a variable
;at different points in history.
;
;-problem: may need an extremely long list
;-solution: construct PART of the list, then delay constructing the rest
;  until the rest is needed.
;-achieved in scheme using STREAMS
;-members of a stream are not evaluated concurrently when stream is created
;-members of a stream are evaluated when they are accessed

;Generating an ordinary list:

;(define list-gen
;  (cons first-term (list-gen other-terms)))

;Generating a stream:

;(define stream-gen
;  (cons first-term (delay (stream-gen other-terms))))

; Expression (delay <exp>) returns a delayed object which will only evaluate
; <exp> when forced.
; 
; -procedure (force <exp>) forces execution of delayed object
;
; My examples:

(define a 50)
(define square (delay
                 (* a a)))
(set! a 25)
(force square)
