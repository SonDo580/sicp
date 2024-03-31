;; Sum of integers from a to b
(define (sum-integer a b)
  (if (> a b)
    0
    (+ a (sum-integer (+ a 1) b))))

;; Sum of cubes of integers from a to b
(define (sum-cubes a b)
  (if (> a b)
    0
    (+ (cube a)
       (sum-cubes (+ a 1) b))))

;; Sum of sequence: 1/1*3 + 1/5*7 + 1/9*11 + ...
;; (this converges to pi/8)
(define (pi-sum a b)
  (if (> a b)
     0
     (+ (/ 1.0 (* a (+ a 2)))
        (pi-sum (+ a 4) b)))) 