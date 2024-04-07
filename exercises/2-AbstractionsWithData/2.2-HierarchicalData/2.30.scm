;; Define a procedure 'square-tree'
;; both directly (i.e., without using any higher-order procedures) 
;; and also by using map and recursion.

;; (define (square-tree tree)
;;   (cond ((null? tree) tree)
;;         ((not (pair? tree)) (* tree tree))
;;         (else (cons (square-tree (car tree))
;;                     (square-tree (cdr tree))))))

;; With 'map'
(define (square-tree tree)
  (define mapFn 
    (lambda (sub-tree)
            (if (pair? sub-tree)
              (square-tree sub-tree)
              (* sub-tree sub-tree))))
  (map mapFn tree))

;; Usage
(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))