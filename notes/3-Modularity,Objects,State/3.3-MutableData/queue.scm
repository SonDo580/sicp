;; First pair of the list
(define (front-ptr queue) (car queue))

;; Last pair of the list
(define (rear-ptr queue) (cdr queue))

(define (set-front-ptr! queue item)
  (set-car! queue item))

(define (set-rear-ptr! queue item) 
  (set-cdr! queue item))

(define (empty-queue? queue)
  (null? (front-ptr queue)))

(define (make-queue) (cons '() '()))

;; Select the item at the front of the queue
(define (front-queue queue)
  (if (empty-queue? queue)
    (error "FRONT called with an empty queue" queue)
    (car (front-ptr queue))))

;; Insert an item (at the back of the queue)
(define (insert-queue! queue item)
  (let ((new-pair (cons item '())))
    (cond ((empty-queue? queue)
           (set-front-ptr! queue new-pair)
           (set-rear-ptr! queue new-pair)
           queue)
          (else 
           (set-cdr! (rear-ptr queue) new-pair)
           (set-rear-ptr! queue new-pair)
           queue))))

;; Delete an item (at the front of the queue)
(define (delete-queue! queue)
  (cond ((empty-queue? queue)
         (error "DELETE! called with an empty queue" queue))
        (else
         (set-front-ptr! queue 
                         (cdr (front-ptr queue)))
         queue)))

;; Test
(define q (make-queue))
(insert-queue! q 'a) ; a
(insert-queue! q 'b) ; a b
(delete-queue! q) ; b
(insert-queue! q 'c) ; b c
(insert-queue! q 'd) ; b c d
(delete-queue! q) ; c d