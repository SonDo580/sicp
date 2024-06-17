(define x (list 'a 'b))
(define z1 (cons x x))
(define z2 (cons (list 'a 'b) (list 'a 'b)))

(define (set-to-wow! x)
  (set-car! (car x) 'wow)
  x)

z1 ; ((a b) a b)
z2 ; ((a b) a b)

(set-to-wow! z1) ; ((wow b) wow b)
(set-to-wow! z2) ; ((wow b) a b)

(eq? (car z1) (cdr z1)) ; true
(eq? (car z2) (cdr z2)) ; false

;; From chapter 2: pairs can be represented purely in terms of procedures
(define (cons x y)
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          (else (error "Undefined operations: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))

;; Implement mutable data objects as procedures
(define (cons x y)
  (define (set-x! v)
    (set! x v))
  (define (set-y! v)
    (set! y v))
  (define (dispatch m)
    (cond ((eq? m 'car) x)
          ((eq? m 'cdr) y)
          ((eq? m 'set-car!) set-x!)
          ((eq? m 'set-cdr!) set-y!)
          (else (error "Undefined operation: CONS" m))))
  dispatch)

(define (car z) (z 'car))
(define (cdr z) (z 'cdr))
(define (set-car! z new-value)
  ((z 'set-car!) new-value)
  z)
(define (set-cdr! z new-value)
  ((z 'set-cdr!) new-value)
  z)