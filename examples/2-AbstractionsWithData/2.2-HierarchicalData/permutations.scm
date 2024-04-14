;; Use nested-mapping to generate all permutations of a set S

;; Plan:
;; - For each item x in S, generate sequence of permutations of S - {x}, and adjoin x to the front of each
;; - Combine these sequences for all x gives all permutations of S

;; Helper
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

;; 'remove' - return all items in a sequence except for a given item
(define (remove item sequence)
  (filter (lambda (x) (not (= x item)))
          sequence))

;; Main
(define (permutations s)
  (if (null? s)
    (list (list))
    (flatmap (lambda (x)
                     (map (lambda (p) (cons x p))
                          (permutations (remove x s))))
             s)))
