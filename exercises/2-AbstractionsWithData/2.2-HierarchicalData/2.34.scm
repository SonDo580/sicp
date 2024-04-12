;; Evaluate polonomials with Horner's rule:
;; an*x^n + (an-1)*x^(n-1) + ... + a1*x + a0
;; = ((an*x + (an-1))x + ...a1)*x + a0

;; In words: start with an, multiply by x, add (an-1), multiply by x, 
;; and so on, until we reach a0

;; Helper
(define (accumulate operation initial sequence)
  (if (null? sequence)
    initial
    (operation (car sequence)
               (accumulate operation initial (cdr sequence)))))

;; Main
(define (horner-eval x coefficient-sequence)
  (accumulate (lambda (this-coeff higher-terms)
                      (+ this-coeff (* x higher-terms)))
              0
              coefficient-sequence))

;; Usage
;; compute 1 + 3x + 5x^3 + x^5 at x = 2
(horner-eval 2 (list 1 3 0 5 0 1))
;; 79