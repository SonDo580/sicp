;; Suppose you are designing a generic arithmetic system 
;; for dealing with the tower of types integer, rational, real, complex. 
;; For each type (except complex), design a procedure 
;; that raises objects of that type one level in the tower. 
;; Show how to install a generic raise operation 
;; that will work for each type (except complex).

;; From :
(define (type-tag datum)
  (if (pair? datum)
    (car datum)
    (error "Bad tagged datum: TYPE-TAG" datum)))

(define (contents datum)
  (if (pair? datum)
     (cdr datum)
     (error "Bad tagged datum: CONTENTS" datum)))

(define (attach-tag type-tag contents)
  (cons type-tag contents))

(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))

;; Handle integers
(define (install-integer-package)
  ;; internal procedure - raise
  (define (raise x)
    (make-rational x 1))

  ;; interface to the rest of the system   
  (define (tag x) (attach-tag 'integer x))
  (put 'make 'integer
       (lambda (x) (tag x)))
  ;; interface the raise procedure
  (put 'raise 'integer raise)
  'done)

;; Create integers
(define (make-integer n)
  ((get 'make 'integer) n))

;; Handle rational numbers
(define (install-rational-package)
  ;; internal procedures
  (define (numer x) (car x))
  (define (denom x) (cdr x))
  (define (make-rat n d)
    (let (g (gcd n d))
      (cons (/ n g) (/ d g))))
  
  ;; internal procedure - raise
  (define (raise x)
    (make-real (/ (numer x) (denom x))))

  ;; interface to the rest of the system 
  (define (tag x) (attach-tag 'rational x))
  (put 'make 'rational
       (lambda (n d) (tag (make-rat n d))))
  ;; interface the raise procedure
  (put 'raise 'rational raise)
  'done)

;; Create rational numbers
(define (make-rational n d)
  ((get 'make 'rational) n d))

;; Handle real numbers
(define (install-integer-package)
  ;; internal procedure - raise
  (define (raise x)
    (make-complex-from-real-imag (contents x) 0))

  ;; interface to the rest of the system 
  (define (tag x) (attach-tag 'real x))
  (put 'make 'real
       (lambda (x) (tag x)))
  ;; interface the raise procedure
  (put 'raise 'real raise)
  'done)

;; Create real
(define (make-integer n)
  ((get 'make 'real) n))

;; COMPLEX NUMBERS
;; Rectangular form
(define (install-rectangular-package)
  ;; internal procedures
  (define (real-part z) (car z))
  (define (imag-part z) (cdr z))
  (define (make-from-real-imag x y) (cons x y))
  (define (magnitude z)
    (sqrt (+ (square (real-part z))
             (square (imag-part z)))))
  (define (angle z)
    (atan (imag-part z) (real-part z)))
  (define (make-from-mag-ang r a)
    (cons (* r (cos a)) (* r (sin a))))

  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'rectangular x))
  (put 'real-part '(rectangular) real-part)
  (put 'imag-part '(rectangular) imag-part)
  (put 'magnitude '(rectangular) magnitude)
  (put 'angle '(rectangular) angle)
  (put 'make-from-real-imag 'rectangular
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'rectangular
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Polar form
(define (install-polar-package)
  ;; internal procedures   
  (define (magnitude z) (car z))
  (define (angle z) (cdr z))
  (define (make-from-mag-ang r a) (cons r a))
  (define (real-part z) (* (magnitude z) 
                           (cos (angle z))))
  (define (imag-part z) (* (magnitude z)
                           (sin (angle z))))
  (define (make-from-real-imag x y)
    (cons (sqrt (+ (square x) (square y)))
          (atan y x)))
  
  ;; interface to the rest of the system
  (define (tag x) (attach-tag 'polar x))
  (put 'real-part '(polar) real-part)
  (put 'imag-part '(polar) imag-part)
  (put 'magnitude '(polar) magnitude)
  (put 'angle '(polar) angle)
  (put 'make-from-real-imag 'polar
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'polar
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Generic selectors
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; INSTALL COMPLEX ARITHMETIC PACKAGE
(define (install-complex-package)
  ;; imported procedures from rectangular and polar packages
  (define (make-from-real-imag x y)
    ((get 'make-from-real-imag 'rectangular) x y))
  (define (make-from-mag-ang r a)
    ((get 'make-from-mag-ang 'polar) r a))

  ;; interface to the rest of the system
  (define (tag z) (attach-tag 'complex z))
  (put 'make-from-real-imag 'complex
       (lambda (x y) (tag (make-from-real-imag x y))))
  (put 'make-from-mag-ang 'complex
       (lambda (r a) (tag (make-from-mag-ang r a))))
  'done)

;; Construct complex numbers
(define (make-complex-from-real-imag x y)
  ((get 'make-from-real-imag 'complex) x y))
(define (make-complex-from-mag-ang r a)
  ((get 'make-from-mag-ang 'complex) r a))

;; Generic raise procedure
(define (raise x) (apply-generic 'raise x))

