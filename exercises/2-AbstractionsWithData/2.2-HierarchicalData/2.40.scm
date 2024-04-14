;; Define 'unique-pairs': generates sequence of pairs (i, j) with 1 ≤ j < i ≤ n
;; Simplify 'prime-sum-pairs' (see example/2.2/nested-mapping)

;; Helper
(define (enumerate-interval low high)
  (if (> low high)
    (list)
    (cons low
          (enumerate-interval (+ low 1) high))))

(define (accumulate operation initial sequence)
  (if (null? sequence)
    initial
    (operation (car sequence)
               (accumulate operation initial (cdr sequence)))))

(define (filter predicate sequence)
  (cond ((null? sequence) (list))
        ((predicate (car sequence))
         (cons (car sequence)
               (filter predicate (cdr sequence))))
        (else (filter predicate (cdr sequence)))))

(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; 'unique-pairs' procedure
(define (unique-pairs n)
  (flatmap (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Simplify 'prime-sum-pairs' (see example/2.2/nested-mapping)
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum?
               (unique-pairs n))))
