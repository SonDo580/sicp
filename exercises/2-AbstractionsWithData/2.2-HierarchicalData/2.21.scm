;; Thee procedure square-list takes a list of numbers as argument 
;; and returns a list of the squares of those numbers.

(define (square-list-1 items)
  (if (null? items)
    items
    (cons (* (car items) (car items))
          (square-list-1 (cdr items)))))

(define (square-list-2 items)
  (map (lambda (x) (* x x))
       items))

(square-list-1 (list 1 2 3 4))
(square-list-2 (list 1 2 3 4))
