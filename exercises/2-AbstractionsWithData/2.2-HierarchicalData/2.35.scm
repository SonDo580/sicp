;; count-leaves procedure (in example/2.2/tree.scm)
;; (define (count-leaves tree)
;;   (cond ((null? tree) 0)
;;         ((not (pair? tree)) 1)
;;         (else (+ (count-leaves (car tree))
;;                  (count-leaves (cdr tree))))))

;; Helper
(define (accumulate operation initial sequence)
  (if (null? sequence)
    initial
    (operation (car sequence)
               (accumulate operation initial (cdr sequence)))))

;; Redefine 'count-leaves' as an accumulation
(define (count-leaves tree)
  (accumulate +
              0 
              (map (lambda (node)
                           (cond ((null? node) 0)
                                 ((pair? node) (count-leaves node))
                                 (else 1)))
                   tree)))

;; Usage
(define x (cons (list 1 2) (list 3 4)))
;; ((1 2) 3 4)
(define y (list x x))
;; (((1 2) 3 4) ((1 2) 3 4))
(count-leaves y) 
;; 8