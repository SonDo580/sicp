;; Find roots of f(x) = 0 by half-interval methodm, where f is a continuous function
;; Idea: given points a and b such that f(a) < 0 < f(b), f must have at least 1 zero between a and b
;; Strategy: keep evaluating f at the midpoint and compare with 0 until the interval is close enough

(define (search f neg-point pos-point)
  (let ((mid-point (average neg-point pos-point)))
    (if (close-enough? neg-point pos-point)
      mid-point
      (let ((test-value (f mid-point))) 
        (cond ((positive? test-value)
               (search f neg-point mid-point))
              ((negative? test-value)
               (search f mid-point pos-point))
              (else mid-point)))))) 

(define (close-enough? x y)
  (< (abs (- x y)) 0.001))

(define (average x y)
  (/ (+ x y) 2))

(define (half-interval-method f a b)
  (let ((a-value (f a))
        (b-value (f b)))
    (cond ((and (negative? a-value) (positive? b-value))
           (search f a b))
          ((and (negative? b-value) (positive? a-value))
           (search f b a))
          (else
           (error "Values are not of opposite sign!" a b)))))

;; approximate pi as the root between 2 and 4 of sin x = 0
(half-interval-method sin 2.0 4.0)

;; search for a root of the equation x^3 - 2x - 3 = 0 between 1 and 2
(half-interval-method (lambda (x) (- (* x x x) (* 2 x) 3)) 
                      1.0
                      2.0)