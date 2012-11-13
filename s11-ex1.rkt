#lang scheme

(require racket/mpair)
(define x (mcons 1 2))
x
(mcons 1 2)
(set-mcar! x 6)
x
(set-mcdr! x 4)
x
(mcons 6 4)
" -- -- -- -- - - - -"
