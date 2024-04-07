;; Scale-tree procedure: multiply each leaves with a factor
(define (scale-tree tree factor)
  (cond ((null? tree) tree)
        ((not (pair? tree)) (* tree factor))
        (else (cons (scale-tree (car tree) factor)
                    (scale-tree (cdr tree) factor)))))

;; With 'map'
(define (scale-tree tree factor)
  (define mapFn
    (lambda (sub-tree)
            (if (pair? sub-tree)
              (scale-tree sub-tree factor)
              (* sub-tree factor))))
  (map mapFn tree))

(scale-tree (list 1 (list 2 (list 3 4) 5) (list 6 7)) 10)
;; (10 (20 (30 40) 50) (60 70))