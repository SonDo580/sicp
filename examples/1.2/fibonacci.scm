;; Recursive
(define (fib n)
  (cond ((n = 0) 0)
        ((n = 1) 1)
        (else (+ (fib (- n 1))
                 (fib (- n 2))))))

;; Iterative
(define (fib-iter a b count)
  (if (= count 0)
    b
    (fib-iter (+ a b) a (- count 1))))

(define (fib n)
  (fib-iter 1 0 n))
