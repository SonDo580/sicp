;; f(x) = x ==> x is a fixed point of function f
;; For some f, we can start with an initial guess then apply f repeatedly
;; f(x), f(f(x)), f(f(f(x))),...
;; until the value doesn't change very much

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

;; approximate the fixed point of the cosine function
(fixed-point cos 1.0)

;; find a solution to the equation y = sin(y) + cos(y)
(fixed-point (lambda (y) (+ (sin y) (cos y))) 1.0)
