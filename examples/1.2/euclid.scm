;; Euclid's algorithm to find greatest common divisor:
;; GCD(a,b) = GCD(b,r)    r is the remainder of a divided by b

;; Iterative process
(define (gcd a b)
  (if (= b 0)
    a
    (gcd b (remainder a b))))

;; Lamé's theorem: bk >= Fib(k)
;; (see the proof on page 64)