;; Find magnitude of the complex number (3, 4)
;; => 5 (in retangular form) or 3 (in polar form)
;; => we need a way to distinguish data 
;; => use type tag - the symbol 'rectangular or 'polar - as part of z

;; Extract tag from a data object
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

;; Extract actual content from a data object
(define (contents datum)
  (if (pair? datum)
     (cdr datum)
     (error "Bad tagged datum: CONTENTS" datum)))

;; Produce a tagged data object
(define (attach-tag type-tag contents)
  (cons type-tag contents))

;; Predicates to recognize rectangular and polar form
(define (rectangular? z)
  (eq? (type-tag z) 'rectangular))

(define (polar? z)
  (eq? (type-tag z) 'polar))

;; Modify code so that 2 representations can coexist in the same system

;; Revised rectangular form
(define (real-part-rectangular z) (car z))

(define (imag-part-rectangular z) (cdr z))

(define (magnitude-rectangular z)
  (sqrt (+ (square (real-part-rectangular z))
           (square (imag-part-rectangular z)))))

(define (angle-rectangular z)
  (atan (imag-part-rectangular z) (real-part-rectangular z)))

(define (make-from-real-imag-rectangular x y) 
  (attach-tag 'rectangular (cons x y)))

(define (make-from-mag-ang-rectangular r a)
  (attach-tag 'rectangular (cons (* r (cos a)) (* r (sin a)))))

;; Revised polar form
(define (real-part-polar z)
  (* (magnitude-polar z) (cos (angle-polar z))))

(define (imag-part-polar z)
  (* (magnitude-polar z) (sin (angle-polar z))))

(define (magnitude-polar z) (car z))

(define (angle-polar z) (cdr z))

(define (make-from-mag-ang-polar r a)
  (attach-tag 'polar (con r a)))

(define (make-from-real-imag-polar x y)
  (attach-tag 'polar (cons (sqrt (+ (square x) (square y)))
                           (atan y x))))

;; Generic selectors: check tag and call the appropriate procedure
(define (real-part z)
  (cond ((rectangular? z) 
         (real-part-rectangular (contents z)))
        ((polar? z) 
         (real-part-polar (contents z)))
        (else (error "Unknown type: REAL-PART" z))))

(define (imag-part z)
  (cond ((rectangular? z) 
         (imag-part-rectangular (contents z)))
        ((polar? z) 
         (imag-part-polar (contents z)))
        (else (error "Unknown type: IMAG-PART" z))))

(define (magnitude z)
  (cond ((rectangular? z)
         (magnitude-rectangular (contents z)))
        ((polar? z)
         (magnitude-polar (contents z)))
        (else (error "Unknown type: MAGNITUDE" z))))

(define (angle z)
  (cond ((rectangular? z)
         (angle-rectangular (contents z)))
        ((polar? z)
         (angle-polar (contents z)))
        (else (error "Unknown type: ANGLE" z))))

;; Same implementation for arithmetic operations
;; Because the selectors they call are generic

;; Constructors
;; - use rectangular form when we have real and imaginary parts
;; - use polar form when we have magnitude and angle
(define (make-from-real-imag x y)
  (make-from-real-imag-rectangular x y))

(define (make-from-mag-ang r a)
  (make-from-mag-ang-polar r a))