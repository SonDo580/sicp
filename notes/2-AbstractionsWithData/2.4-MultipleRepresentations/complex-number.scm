;; We have 2 procedures for constructing complex numbers
;; - make-from-real-imag (from real and imaginary parts)
;; - make-from-mag-ang (from magnitude and angle)

;; Operations on complex numbers - 4 selectors
;; - real-part 
;; - imag-part 
;; - magnitude
;; - angle

;; Relations:
;; x = r*cos(A)
;; y = r*sin(A)
;; r = sqrt(x^2 + y^2)
;; A = arctan(y, x)

;; Represent complex numbers in rectangular form
(define (real-part z) (car z))

(define (imag-part z) (cdr z))

(define (magnitude z)
  (sqrt (+ (square (real-part z))
           (square (imag-part z)))))

(define (angle z)
  (atan (imag-part z) (real-part z)))

(define (make-from-real-imag x y) (cons x y))

(define (make-from-mag-ang r a)
  (cons (* r (cos a))
        (* r (sin a))))

;; Represent complex numbers in polar from
(define (real-part z)
  (* (magnitude z) (cos (angle z))))

(define (imag-part z)
  (* (magnitude z) (sin (angle z))))

(define (magnitude z) (car z))

(define (angle z) (cdr z))

(define (make-from-mag-ang r a) (con r a))

(define (make-from-real-imag x y)
  (cons (sqrt (+ (square x) (square y)))
        (atan y x)))

;; For easy manipulation, we should:
;; - add and subtract in terms of real and imaginary parts
;; - multiply and divide in terms of magnitude and angles

;; (a + bi) + (c + di) = (a + b) + (c + d)i
(define (add-complex z1 z2)
  (make-from-real-imag (+ (real-part z1) (real-part z2))
                       (+ (imag-part z1) (imag-part z2))))

;; (a + bi) - (c + di) = (a - b) + (c - d)i
(define (sub-complex z1 z2)
  (make-from-real-imag (- (real-part z1) (real-part z2))
                       (- (imag-part z1) (imag-part z2))))

;; A*e^ia * B*e^ib = A*B*e^i(a+b)
(define (mul-complex z1 z2)
  (make-from-mag-ang (* (magnitude z1) (magnitude z2))
                     (+ (angle z1) (angle z2))))

;; A*e^ia / B*e^ib = (A/B*)e^i(a-b)
(define (div-complex z1 z2)
  (make-from-mag-ang (/ (magnitude z1) (magnitude z2))
                     (- (angle z1) (angle z2))))
