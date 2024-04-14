;; Write a procedure to find all ordered triples
;; of distinct positive integers (i, j, k) 
;; less than or equal to a given integer n 
;; that sum to a given integer s.

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

;; Calculate sum of items of a sequence
(define (sum-seq seq)
  (accumulate + 0 seq))

;; Check if sum of items of a sequence is equal to s
(define (sum-to-s? s seq)
  (= s (sum-seq seq)))

;; Geerate sequence of unique pairs (i, j)
;; 1 <= j < i <= n
(define (unique-pairs n)
  (flatmap (lambda (i)
                   (map (lambda (j) (list i j))
                        (enumerate-interval 1 (- i 1))))
           (enumerate-interval 1 n)))

;; Generate sequence of unique triples (i, j, k)
;; 1 <= k < j < i <= n
(define (unique-triples n)
  (flatmap (lambda (i)
                   (map (lambda (jk-pair) (cons i jk-pair))
                        (unique-pairs (- i 1))))                   
           (enumerate-interval 1 n)))

(unique-triples 4)

;; Main procudure (combine all)
(define (triples-sum-to-s s n)
  (filter (lambda (triple) (sum-to-s? s triple))
          (unique-triples n)))

;; Usage
(triples-sum-to-s 12 10)
