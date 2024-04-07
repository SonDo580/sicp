;; Count number of leaves in a tree
(define (count-leaves tree)
  (cond ((null? tree) 0)
        ((not (pair? tree) 1))
        (else (+ (count-leaves (car tree))
                 (count-leaves (cdr tree))))))

(define x (cons (list 1 2) (list 3 4)))
;; ((1 2) 3 4)

(length x) ; 3
(count-leaves x) ; 4

(define y (list x x))
;; (((1 2) 3 4) ((1 2) 3 4))

(length y) ; 2
(count-leaves y) ; 8

