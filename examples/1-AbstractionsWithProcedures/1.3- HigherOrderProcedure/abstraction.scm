;; We introduced 2 ways of computing square root as fixed points
;; (see return.scm and newton-method.scm)

;; We can define another higher-order procedure
(define (fixed-point-of-transform g transform guess)
  (fixed-point (transform g) guess))

;; Express the average-damping method with fixed-point-of-transform
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (/ x y))
                            average-damp
                            1.0))

;; Express the newton's method way with fixed-point-of-transform
(define (sqrt x)
  (fixed-point-of-transform (lambda (y) (- (square y) x))
                            newton-transform 
                            1.0))


;; HELPERS 
;; average-damp
(define (average-damp f)
  (lambda (x) (average x (f x))))

;; newton-transform
(define (newton-transform g)
  (lambda (x) (- x
                 (/ (g x)
                    ((deriv g) x)))))

;; fixed-point
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