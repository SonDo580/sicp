(define (sum a b term next)
  (if (> a b)
    0
    (+ (term a)
       (sum (next a) b term next))))

;; Compute pi-sum
(define (pi-sum a b)
  (sum a
       b
       (lambda (x) (/ 1.0 (* x (+ x 2))))
       (lambda (x) (+ x 4))))

;; Compute integral
(define (integral f a b dx)
  (* dx
     (sum a
          b
          (lambda (x) (f (+ x (/ dx 2.0))))
          (lambda (x) (+ x dx)))))

;; => lambda is used to create procedures with no name

