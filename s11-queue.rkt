#lang racket
(require racket/mpair)

"we can construct efficient data structures with mutable pairs and lists. Example: queues.
Queue: sequence where items inserted at back and deleted from front.

Queue should have following operations:
-Constructor
-Accessor for front
-Accessor for back
-Test for empty queue
-Mutator to insert at back and return the modified queue
-Mutator to delete at front and return the modified queue

To maintain efficiency, we'll represent the queue as a list WITH a pointer
that indicates rear pair, for fast access."

"constructor:"
(define (make-queue)
  (mcons '() '()))

"accessors:"
(define (front-ptr queue)
  (mcar queue))

(define (rear-ptr queue)
  (mcdr queue))

"mutators:"
(define (set-front-ptr! queue item)
  (set-mcar! queue item))

(define (set-rear-ptr! queue item)
  (set-mcdr! queue item))

"empty test:"
(define (empty-queue? queue)
  (null? (front-ptr queue)))

"select at front:"
(define (front-queue queue)
  (if (empty-queue? queue)
      (error "NO ITEM, FRONT IS EMPTY" queue)
      (car (front-ptr queue))))  ;;NOTE: why is this not mcar?

"insert algorithm:
-create a new pair whose car is item to be inserted and cdr is null
-if queue was empty, set front and rear pointers of queue to be new pair
-else modify final pair in queue to point to new pair and set rear 
pointer to new pair"

; NOTE!!! this can't be correct.

(define (insert-queue! queue item)
  (let ((new-pair (mcons item '())))
    (cond ((empty-queue? queue)  ;;why is this cond and not if
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else
           (set-mcdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

"delete from front:
-just modify front pointer so it points at second item in queue.
-keep in mind empty queue case"
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "EMPTY QUEUE, CAN'T DELETE"))
         (else
          (set-front-ptr! queue
                          (mcdr (front-ptr queue)))
          queue)))
"- - - - - - -  -"
"Now let's test the queue"
(define new-queue (make-queue))
"Is the queue empty?"
(empty-queue? new-queue)
new-queue
"Add an item."
(insert-queue! new-queue 5)
(front-ptr new-queue)
(rear-ptr new-queue)
"Remove the item."
(delete-queue! new-queue)
new-queue
"Test for empty again. Tip: should be empty"
(empty-queue? new-queue)
"Add 5 values"
(insert-queue! new-queue 1)
(insert-queue! new-queue 6)
(insert-queue! new-queue 9)
(insert-queue! new-queue 4)
(insert-queue! new-queue 6)
new-queue
"Remove 3 values"
(delete-queue! new-queue)
(delete-queue! new-queue)
(delete-queue! new-queue)
new-queue