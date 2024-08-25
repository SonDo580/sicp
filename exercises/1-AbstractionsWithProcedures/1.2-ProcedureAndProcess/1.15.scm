;; The sine of an angle (specified in radians) can be computed by 
;; making use of the approximation sin x ≈ x if x is sufficiently small, 
;; and the trigonometric identity sin x = 3*sin(x/3) - 4*(sin(x/3))^3
;; to reduce the size of the argument of sin.

(define (cube x) (* x x x))
(define (p x) (- (* 3 x) (* 4 (cube x))))
(define (sine theta)
  (if (<= (abs theta) 0.1)
    theta
    (p (sine (/ theta 3.0)))))

;; a. How many times is p applied when (sine 12.15) is evaluated?

;; (sine 12.15)
;; (p (sine 4.05))
;; (p (p (sine 1.35)))
;; (p (p (p (sine 0.45))))
;; (p (p (p (p (sine 0.15)))))
;; (p (p (p (p (p (sine 0.05))))))
;; 
;; => p is applied 5 times 

;; b. What is the order of growth in space and number of
;; steps (as a function of a) used by the process generated
;; by the sine procedure when (sine a) is evaluated?

;; p is applied k times until a / 3^k < 0.1
;; => k > log3(10a)
;; => number of steps is O(log n)
;; => space complexity is also O(log n) 
;;    (must maintain the stack for p calls - see part a)