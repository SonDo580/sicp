;; if g(x) is a differentiable function
;; then a solution to g(x) = 0 is a fixed point of the function f(x)
;; where f(x) = x - g(x) / Dg(x)
;; Dg(x): the derivative of g evaluated at x

;; Elementary calculus books usually describe Newton’s method
;; in terms of the sequence of approximations xn+1 = xn - g(xn) / Dg(xn)

;; Express derivative
;; Dg(x) = (g(x + dx) - g(x)) / dx
(define (deriv g)
  (define dx 0.00001)
  (lambda (x)
          (/ (- (g (+ x dx))
                (g x))
             dx)))

;; Express Newton's method as a fixed-point process
(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))

(define (newton-method g guess)
  (fixed-point (newton-transform g) guess))

;; Compute square root of x
;; y^2 = x <=> y^2 - x = 0
;; We use Newtons's method to find a root of g(y) = y^2 - x
(define (sqrt x)
  (newton-method (lambda (y) (- (square y) x)) 
                 1.0))

;; Helpers: fixed-point
(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))


