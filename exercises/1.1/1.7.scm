;; Finding square root with Newton's method
;; Use alternative implementation for good-enough?  

(define (average x y)
  (/ (+ x y) 2))

(define (sqrt x)
  (define (sqrt-iter guess)
    (if (good-enough? guess)
      guess
      (sqrt-iter (improve guess))))
  
  (define (improve guess) (average guess (/ x guess)))
  
  (define (good-enough? guess)
    (define delta (abs (- (improve guess) guess)))
    (< (/ delta guess) 0.001))
  
  (define initial-guess 1.0)
  
  (sqrt-iter initial-guess))

(sqrt 0.001)