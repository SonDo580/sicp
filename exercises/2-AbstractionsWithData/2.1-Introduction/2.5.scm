;; Represent pairs of non-negative integers 
;; using only numbers and arithmetic operations 
;; by encoding the pair (a, b) as a single integer 2^a*3^b. 
;; Give the corresponding definitions of the procedures
;; cons, car, and cdr.

;; Helper
(define (exp base power)
  (define (iter result current)
    (if (= current 0)
      result
      (iter (* result base) (- current 1))))
  (iter 1 power))

;; n = 2^a*3^b
;; 2^a is not divided by 3 
;; => to get b, keep dividing n by 3 until the remainder is not 0
;; Use similar method to get a 

(define (divides? dividend divisor)
  (= (remainder dividend divisor) 0))

(define (component n divisor)
  (define (iter current power)
    (if (divides? current divisor)
      (iter (/ current divisor) (+ power 1))
      power))
  (iter n 0))

;; Main
(define (cons a b)
  (* (exp 2 a) (exp 3 b))) 

(define (car n)
  (component n 2))

(define (cdr n)
  (component n 3))

;; Usages
(define n (cons 3 4))
(car n)
(cdr n)