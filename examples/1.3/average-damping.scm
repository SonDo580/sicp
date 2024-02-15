;; Formulate square-root computation as a fixed-point search
;; sqrt of x <=> find y such that y^2 = x
;; Look for a fixed point of the function y = x / y

;; Initial attempt
(define (sqrt x)
  (fixed-point (lambda (y) (/ x y))
               1.0))
;; This search does not converge
;; Explain: y1 -> y2 = x / y1 -> y3 = x / y2 = y1 (repeated)

;; To control the oscillation, use a different next-guess (average y and x / y)
;; => Look for a fixed point of the function y = 1/2 * (y + x/y)
(define (sqrt x)
  (fixed-point (lambda (y) (average y (/ x y)))
               1.0))
;; y = 1/2 * (y + x/y) is a simple transformation of the equation y = x/y
;; To derive it, add y to both sides of the equation and divide by 2

;; Helpers
(define tolerance 0.00001)

(define (close-enough? v1 v2)
  (< (abs (- v1 v2)) tolerance))

(define (average x y)
  (/ (+ x y) 2))

(define (fixed-point f first-guess)
  (define (try guess)
    (let ((next (f guess)))
      (if (close-enough? guess next)
        next
        (try next))))
  (try first-guess))