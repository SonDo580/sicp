;; (from exercise 1.15)
;; The sine of an angle (specified in radians) can be computed by 
;; making use of the approximation sin x ≈ x if x is sufficiently small, 
;; and the trigonometric identity sin x = 3*sin(x/3) - 4*(sin(x/3))^3
;; to reduce the size of the argument of sin.

(define (cube x)
  (* x x x))

(define (small-enough? x)
  (not (> (abs x) 0.1)))

(define (p x)
  (- (* 3 x)
     (* 4 (cube x))))

(define (sine angle)
  (if (small-enough? angle)
    angle
    (p (sine (/ angle 3)))))