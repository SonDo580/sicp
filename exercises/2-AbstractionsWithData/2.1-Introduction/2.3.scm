;; Represent rectangles in a plane

;; For simplicity, assume that: 
;; - rectangles have sides parallel to x-axis and y-axis
;; - top-left and bottom-right are valid 

(define (make-point x y) (cons x y))
(define (x-point p) (car p))
(define (y-point p) (cdr p))

(define (make-segment p1 p2) (cons p1 p2))
(define (start-segment s) (car s))
(define (end-segment s) (cdr s))

(define (make-rectangle top-left bottom-right)
  (cons top-left bottom-right))

(define (top-left rectangle) (car rectangle))
(define (bottom-right rectangle) (cdr rectangle))

(define (x-side rectangle)
  (let ((tl (top-left rectangle))
        (br (bottom-right rectangle)))
    (- (x-point br) (x-point tl))))

(define (y-side rectangle)
  (let ((tl (top-left rectangle))
        (br (bottom-right rectangle)))
    (- (y-point tl) (y-point br))))

(define (perimeter rectangle)
  (let ((side1 (x-side rectangle))
        (side2 (y-side rectangle)))
    (* 2 (+ side1 side2))))

(define (area rectangle)
  (let ((side1 (x-side rectangle))
        (side2 (y-side rectangle)))
    (* side1 side2)))

(define tl (make-point 2 2))
(define br (make-point 6 0))
(define rect (make-rectangle tl br))
(perimeter rect)
(area rect)
