;; Compute the exponentiation of a number: b^n

;; Linear recursive process with Θ(n) steps and Θ(n) space
(define (expt b n)
  (if (= n 0)
    1
    (* b (expt b (- n 1)))))

;; Iterative process with Θ(n) steps and Θ(1) space
(define (expt b n)
  (define (expt-iter counter product)
    (if (= counter 0)
      product
      (expt-iter (- counter 1)
                 (* b product))))
  (expt-iter n 1))

;; Better: use successive squaring 
;; b^n = (b^(n/2))^2 if n is even
;; b^n = b * b^(n-1) if n is odd
(define (even? n)
  (= (remainder n 2) 0))

(define (fast-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (fast-expt b (/ n 2))))
        (else (* b (fast-expt b (- n 1))))))

;; Θ (log n) in both number of steps and space
;; computing b^2n requires 1 more multiplcation than computing b^n



