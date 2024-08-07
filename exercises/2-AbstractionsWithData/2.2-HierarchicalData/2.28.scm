;; Write a procedure 'fringe' that takes as argument a tree 
;; (represented as a list) and returns a list whose
;; elements are all the leaves of the tree arranged in left-to-right order. 

(define (fringe tree)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (list tree))
        (else (append (fringe (car tree))
                      (fringe (cdr tree))))))

;; Usage
(define x (list (list 1 2) (list 3 4)))

(fringe x)
;; (1 2 3 4)

(fringe (list x x))
;; (1 2 3 4 1 2 3 4)
