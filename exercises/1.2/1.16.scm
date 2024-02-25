;; Design a procedure that evolves an iterative exponentiation process 
;; that uses successive squaring and uses a logarithmic number of steps, 
;; as does 'fast-expt' (see example 'exponentiation')

;; For reference: recursive process 
(define (recursive-expt b n)
  (cond ((= n 0) 1)
        ((even? n) (square (recursive-expt b (/ n 2))))
        (else (* b (recursive-expt b (- n 1))))))

;; Iterative process
;; n is even: b^k = (b^2)^(k/2)
;; n is odd: b^k = b * b^(k-1)
(define (iterative-expt b n)
  (define (iter counter base product)
    (cond ((= 0 counter) product)
          ((even? counter) (iter (/ counter 2) (square base) product))
          (else (iter (- counter 1) base (* base product)))))
  (iter n b 1))

;; ANALYZE:
;; If n = 0 -> return 1 (base case)
;; If n = 2^p (p is an integer):
;; - 'iter' will keep evaluating the 'even' case, until 'counter' is 1
;; - 'product' is still 1, whereas the current base is now b^n
;; - the next 'counter' is 0, so the result is b^n * 1 = b^n
;; Normal case: 
;; - accumulate the current base every time counter becomes odd
;; - example: 
;; >  3^10
;; > (3^2)^5
;; >  3^2 * (3^2)^4 
;; >  3^2 * (3^4)^2
;; >  3^2 * 3^8      

;; HELPERS
(define (even? n)
  (= (remainder n 2) 0))