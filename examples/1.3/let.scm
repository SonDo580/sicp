;; Use 'let' to create local variables

;; f(x,y) = x(1+xy)^2 + y(1-y) + (1+xy)(1-y)

;; With auxiliary procedure
(define (f x y)
  (define (helper a b)
    (+ (* x (square a))
       (* y b)
       (* a b)))
  (helper (+ 1 (* x y))
          (- 1 y)))

;; With lambda expression
(define (f x y)
  ((lambda (a b)
           (+ (* x (square a))
              (* y b)
              (* a b))) 
   (+ 1 (* x y))
   (- 1 y)))

;; With 'let'
(define (f x y)
  (let ((a (+ 1 (* x y)))
        (b (- 1 y)))
    (+ (* x (square a))
       (* y b)
       (* a b))))

;; A let expression is simply syntactic sugar for the underlying lambda application

;; Variables' values are computed outside the 'let'
;; (let ((x 3)
;;       (y (+ x 2)))
;;   (* x y))
;; ==> y = outer_x + 2 (not 3 + 2)

;; Another way: With internal definitions (prefer 'let')
(define (f x y)
  (define a (+ 1 (* x y)))
  (define b (- 1 y))
  (+ (* x (square a))
     (* y b)
     (* a b)))
