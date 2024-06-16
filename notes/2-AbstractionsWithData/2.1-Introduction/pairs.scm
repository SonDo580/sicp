;; Implement cons, car, cdr with procedures
;; condition: for any objects x and y, 
;; if z is (cons x y) then (car z) is x and (cdr z) is y.

(define (cons x y)
  (define (dispatch m)
    (cond ((= m 0) x)
          ((= m 1) y)
          (else (error "Argument not 0 or 1: CONS" m))))
  dispatch)

(define (car z) (z 0))
(define (cdr z) (z 1))