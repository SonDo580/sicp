;; Problem: Given a positive integer n, 
;; find all ordered pairs of distinct positive integers i and j, 
;; where 1 ≤ j < i ≤ n, such that i + j is prime.

;; Method:
;; - generate sequence of all ordered pairs of positive integers <= n
;; - filter to select pairs whose sum is prime
;; - for each pair (i, j), produce the triple (i, j, i + j)

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

;; Generate sequence of pairs
;; (accumulate append
;;             (list)
;;             (map (lambda (i)
;;                          (map (lambda (j) (list i j))
;;                               (enumerate-interval 1 (- i 1))))
;;                  (enumerate-interval 1 n)))

;; Generate the above code with this procedure
(define (flatmap proc seq)
  (accumulate append (list) (map proc seq)))

;; Predicate to check pairs whose sum is prime 
(define (prime-sum? pair)
  (prime? (+ (car pair) (cadr pair))))

;; Procdure to construct triple (i, j, i + j)
(define (make-pair-sum pair)
  (list (car pair) (cadr pair) (+ (car pair) (cadr pair))))

;; Combine all steps
(define (prime-sum-pairs n)
  (map make-pair-sum
       (filter prime-sum? 
               (flatmap (lambda (i)
                                (map (lambda (j) (list i j))
                                     (enumerate-interval 1 (- i 1))))
                        (enumerate_interval 1 n)))))
