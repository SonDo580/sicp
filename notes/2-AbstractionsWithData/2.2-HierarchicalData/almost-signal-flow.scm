;; Computes the sum of the squares of the leaves that are odd
(define (sum-odd-squares tree)
  (cond ((null? tree) 0)
        ((not (pair? tree))
         (if (odd? tree) (square tree) 0))
        (else (+ (sum-odd-squares (car tree))
                 (sum-odd-squares (cdr tree))))))
;; enumerate: tree-leaves
;; filter: odd?
;; map: square
;; accumulate: +, 0

;; Construct a list of all even Fibonacci numbers Fib(k) where k <= n
(define (even-fibs n)
  (define (next k)
    (if (> k n)
      (list)
      (let ((f (fib k)))
        (if (even? f)
          (cons f (next (+ k 1)))
          (next (+ k 1))))))
  (next 0))
;; enumerate: integers
;; filter: fib
;; map: even?
;; accumulate: cons, ()

;; => We will reformulate these procedures in signal-flow.scm 