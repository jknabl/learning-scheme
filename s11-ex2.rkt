#lang racket

(require racket/mpair)

(define x (mcons 1 (mcons 2 (mcons 3 null))))
x
(mcar x)
(mcar (mcdr x))
(define y (mlist 5 6 7 8))
y
(set-mcdr! x 4)
x
"mcar x is"
(mcar x)
"mcdr x is"
(mcdr x)
" - - -- --  - - - -"
"Y is:"
y
(define z (mcdr y))
z
(set-mcar! z 22)
z