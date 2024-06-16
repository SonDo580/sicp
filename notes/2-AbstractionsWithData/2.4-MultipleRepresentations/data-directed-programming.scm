;; dispatching on type: 
;; - checking type and call the appropriate method.
;; 
;; weakness: 
;; - the generic procedures must know about all different representations.
;; - although individual representations can be designed separately,
;;   we still have to guarantee no 2 procedures have the same name.
;; -> the technique for implementing the generic interface is not additive.

;; observation:
;; - whenever we deal with a set of generic operations that are common 
;;   to a set of data -> we are dealing with a 2-dimensional table that
;;   + the possible operations on 1 axis.
;;   + the possible types on another axis.
;;   + the entries are procedures that implement each operation for each type
;; 
;; data-directed programming:
;; - a technique of designing programs to work with such a table directly
;; - to add a new representation package, we only need to add new entries

;; Assume that we have 2 operations for manipulating the table
;; - (put ⟨op⟩ ⟨type⟩ ⟨item⟩) installs the ⟨item⟩ in the table
;; - (get ⟨op⟩ ⟨type⟩) looks up the entry in table, returns false if not found.

;; Example with complex number package

;; Helpers (same as tagged-data.scm)
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

;; Note:
;; - use the list '(rectangular) to allow operations with multiple arguments,
;;   not all of the same types.
;; - the type the constructors are installed under needn't be a list,
;;   because constructor is always use to make an object of 1 particular type. 

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

;; Accessing the table
(define (apply-generic op . args)
  (let ((type-tags (map type-tag args)))
    (let ((proc (get op type-tags)))
      (if proc
        (apply proc (map contents args))
        (error "No method for these types: APPLY-GENERIC"
               (list op type-tags))))))

;; Define the generic selectors with apply-generic
(define (real-part z) (apply-generic 'real-part z))
(define (imag-part z) (apply-generic 'imag-part z))
(define (magnitude z) (apply-generic 'magnitude z))
(define (angle z) (apply-generic 'angle z))

;; Constructors
(define (make-from-real-imag x y)
  ((get 'make-from-real-imag 'rectangular) x y))

(define (make-from-mag-ang r a)
  ((get 'make-from-mag-ang 'polar) r a))
