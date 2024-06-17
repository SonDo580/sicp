;; Represent queue as a procedure with local state

(define (make-queue)
  (let ((front-ptr '())
        (rear-ptr '()))
    (define (empty-queue?)
      (null? front-ptr))
    
    (define (get-queue)
      (cons front-ptr rear-ptr))
    
    (define (front-queue)
      (if (empty-queue?)
        (error "FRONT called with an empty queue" 
               (get-queue))
        (car front-ptr)))
    
    (define (insert-queue! item)
      (let ((new-pair (cons item '())))
        (cond ((empty-queue?)
               (set! front-ptr new-pair)
               (set! rear-ptr new-pair)
               (get-queue))
              (else
               (set-cdr! rear-ptr new-pair)
               (set! rear-ptr new-pair)
               (get-queue)))))

    (define (delete-queue!)
      (cond ((empty-queue?)
             (error "DELETE! called with an empty queue"
                    (get-queue)))
            (else
             (set! front-ptr
                   (cdr front-ptr))
             (get-queue))))

    (define (dispatch m)
      (cond ((eq? m 'delete!) delete-queue!)
            ((eq? m 'insert!) insert-queue!)
            ((eq? m 'front) front-queue)
            ((eq? m 'empty) empty-queue?)
            ((eq? m 'queue) get-queue)
            (else
             (error "Unknown operation: QUEUE" m))))
    dispatch))

;; Test
(define q (make-queue))
((q 'insert!) 'a) ; a
((q 'insert!) 'b) ; a b
((q 'delete!)) ; b
((q 'insert!) 'c) ; b c
((q 'insert!) 'd) ; b c d
((q 'delete!)) ; c d