;; Complete the procedure that generates the set of subsets of a set
(define (subsets s)
  (if (null? s)
    (list (list))
    (let ((rest (subsets (cdr s))))
      (append rest 
              (map (lambda (subset)
                           (cons (car s) subset))
                   rest)))))

;; Usage
(subsets (list 1 2 3))
;; (() (3) (2) (2 3) (1) (1 3) (1 2) (1 2 3))

