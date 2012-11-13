#lang racket
(require racket/mpair)

"mlist? checks to see if a list is mutable"
(mlist? (mcons 1 2))
(mlist? (mlist 1 2 3))
(mlist? (list 1 2 3))

"mlist->list takes a mutable list and returns a non-mutable list"
(define y (mlist 5 6 7))
(mlist->list y)
(define a (mlist->list y))
(mlist? a)
(list? a)

"list->mlist takes a nonmutable list and makes a mutable list"
(define x (list 1 2 3))
(define b (list->mlist x))
(mlist? b)
b
(set-mcar! b 18)
b
(set-mcar! b 1)
b

"mlength returns the number of items in a mutable list"
(mlength (mlist 2 5 6))

"mlist-ref returns the element of the mutable list at the given position, indexed 0-N"
(mlist-ref (mlist 2 5 6) 2)

"mappend takes 2+ mutable lists and combines them, preserving order"
(define g (mlist 1 2 3))
g
(define h (mlist 6 7 8))
h
(define g_plus_h (mappend g h))
g_plus_h

"mappend! appends mutable list by modifying each of the lists. Mutates tail to refer to next list."
(mappend! g h)
g
h

"mreverse takes a mutable list, returns a freshly allocated mutable list"
(define rv (mlist 1 2 3))
rv
(define vr (mreverse rv))
vr
rv

"mreverse! takes a mutable list, reverses the list in-place, returns same list reversed"
(mreverse! rv)
rv
