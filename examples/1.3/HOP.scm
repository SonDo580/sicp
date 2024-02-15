;; Use higher-order procedure 'sum'
(define (sum a b term next)
  (if (> a b)
    0
    (+ (term a)
       (sum (next a) b term next))))

;; Compute sum of cubes
(define (inc n) (+ n 1))

(define (sum-cubes a b)
  (sum a b cube inc))

;; Compute sum of integers
(define (identity x) x)

(define (sum-integers a b)
  (sum a b identity inc))

;; Compute pi-sum
(define (pi-sum a b)
  (define (term x)
    (/ 1.0 (* x (+ x 2))))
  (define (next x)
    (+ x 4))
  (sum a b term next))

;; Approximate definite integral
(define (integral f a b dx)
  (define (term x)
    (f(+ x (/ dx 2.0))))
  (define (add-dx x)
    (+ x dx))
  (* dx
     (sum a b term add-dx)))
