;; SEARCHING FOR DIVISOR

;; - if n is not prime it must have a divisor less than or equal to sqrt(n)
;;   (if d is a divisor of n, then so is n/d. but d and n/d cannot both
;;    be greater than sqrt(n)) 
;; -> number of steps will have order of growth O(sqrt(n)) 

(define (smallest-divisor n) (find-divisor n 2))

(define (find-divisor n test-divisor)
  (cond ((> (square test-divisor) n) n)
        ((divides? test-divisor n) test-divisor)
        (else (find-divisor n (+ test-divisor 1)))))

(define (divides? a b) (= (remainder b a) 0))

(define (prime? n)
  (= n (smallest-divisor n)))