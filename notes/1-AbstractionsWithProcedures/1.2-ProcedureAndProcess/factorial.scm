;; Recursion approach
(define (factorial n)
  (if (= n 0)
    1
    (* n (factorial (- n 1)))))

;; Iterative approach
(define (factorial1 n)
  (define (iter product counter)
    (if (> counter n)
      product
      (iter (* counter product) (+ counter 1))))
  (iter 1 1))
