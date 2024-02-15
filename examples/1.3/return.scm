;; We can use procedures as returned values
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; Re-formulate sqrt procedure
;; (define (sqrt x)
;;   (fixed-point (lambda (y) (average y (/ x y)))
;;                1.0))
(define (sqrt x)
  (fixed-point (average-damp (lambda (y) (/ x y)))
               1.0))

;; Notice that cube root of x is a fixed point of y = x / y^2
(define (cube-root x)
  (fixed-point (average-damp (lambda (y) (/ x (square y))))
               1.0))

;; Helpers
(define (average x y)
  (/ (+ x y) 2))

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