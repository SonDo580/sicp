;; 3.18: Write a program to detect cycle in a list
;; 3.19: Done (this algorithm also takes a constant amount of space) 
(define (detect-cycle lst)
  (define (iter current)
    (cond ((null? current) false)
          ((eq? current lst) true)
          (else (iter (cdr current)))))
  (if (null? lst)
    false
    (iter (cdr lst))))

;; Test
(define (last-pair x)
  (if (null? (cdr x))
    x
    (last-pair (cdr x))))

(define (make-cycle x)
  (set-cdr! (last-pair x) x)
  x)

(define z1 (make-cycle (list 'a 'b 'c)))
(define z2 (list 'a 'b 'c))
(define z3 '())
(detect-cycle z1) ; should be true
(detect-cycle z2) ; should be false
(detect-cycle z3) ; should be false