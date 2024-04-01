;; Represent points and line-segments in a plane

(define (average a b)
  (/ (+ a b) 2))

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (midpoint-segment s)
  (let ((p1 (start-segment s))
        (p2 (end-segment s)))
    (let ((mid-x (average (x-point p1) (x-point p2)))
          (mid-y (average (y-point p1) (y-point p2))))
      (make-point mid-x mid-y))))

(define (print-point p)
  (newline)
  (display "(")
  (display (x-point p))
  (display ",")
  (display (y-point p))
  (display ")"))

;; Usages
(define p1 (make-point 3 4))
(define p2 (make-point 8 10))
(define segment (make-segment p1 p2))
(define midpoint (midpoint-segment segment))

(print-point p1)
(print-point p2)
(print-point midpoint)


