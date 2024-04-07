;; Abstract your answer to Exercise 2.30 to
;; produce a procedure tree-map with the property 
;; that square-tree could be defined as
;; (define (square-tree tree) (tree-map square tree))

(define (tree-map proc tree)
  (define mapFn
     (lambda (sub-tree)
             (if (pair? sub-tree)
               (tree-map proc sub-tree)
               (proc sub-tree))))
   (map mapFn tree))

;; Test
(define (square-tree tree) (tree-map square tree))

(square-tree
 (list 1
       (list 2 (list 3 4) 5)
       (list 6 7)))
;; (1 (4 (9 16) 25) (36 49))
